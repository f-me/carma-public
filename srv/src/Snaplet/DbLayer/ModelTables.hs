{-# LANGUAGE OverloadedStrings #-}

module Snaplet.DbLayer.ModelTables (
    -- * Types
    ModelDesc(..), ModelField(..),
    ModelGroups(..),
    TableDesc(..), TableColumn(..),

    -- * Loading
    loadTables,

    -- * Queries
    createExtend,
    insertUpdate, insert, update,

    -- * Util
    tableByModel,
    addType
    ) where

import Prelude hiding (log)

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.CatchIO
import Data.List hiding (insert)
import Data.Text (Text)
import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import System.FilePath
import System.Directory
import System.Log

import qualified Database.PostgreSQL.Simple as P

data ModelDesc = ModelDesc {
    modelName :: String,
    modelFields :: [ModelField] }
        deriving (Eq, Ord, Read, Show)

data ModelField = ModelField {
    fieldName :: String,
    fieldType :: Maybe String,
    fieldGroup :: Maybe String }
        deriving (Eq, Ord, Read, Show)

instance FromJSON ModelField where
    parseJSON (Object v) = ModelField <$>
        v .: "name" <*>
        v .:? "type" <*>
        v .:? "groupName"
    parseJSON _ = empty

instance FromJSON ModelDesc where
    parseJSON (Object v) = ModelDesc <$>
        v .: "name" <*>
        v .: "fields"
    parseJSON _ = empty

data ModelGroups = ModelGroups {
    modelGroups :: M.Map String [ModelField] }
        deriving (Show)

instance FromJSON ModelGroups where
    parseJSON (Object v) = (ModelGroups . M.fromList) <$> (mapM parseField $ HM.toList v) where
        parseField (nm, val) = do
            flds <- parseJSON val
            return (T.unpack nm, flds)
    parseJSON _ = empty

data TableDesc = TableDesc {
    tableName :: String,
    tableModel :: String,
    tableParent :: [String],
    tableFields :: [TableColumn] }
        deriving (Eq, Ord, Read, Show)

data TableColumn = TableColumn {
    columnName :: String,
    columnType :: String }
        deriving (Eq, Ord, Read, Show)

-- | Load all models, and generate table descriptions
loadTables :: String -> String -> IO [TableDesc]
loadTables base field_groups = do
    gs <- loadGroups field_groups
    ms <- fmap (filter $ isSuffixOf ".js") $ getDirectoryContents base
    liftM (mergeServices . map addId) $ mapM (loadTableDesc gs base) ms

-- | Create or extend table
createExtend :: (MonadLog m) => P.Connection -> TableDesc -> m ()
createExtend con tbl = scope "createExtend" $ do
    exec $ createTableQuery tbl
    mapM_ exec $ extendTableQueries tbl
    where
        exec q = ignoreError $ scope "query" $ do
            log Trace $ fromString q
            liftIO $ P.execute_ con (fromString q)
            return ()

-- | Insert or update data into table
insertUpdate :: MonadLog m => P.Connection -> TableDesc -> C8.ByteString -> M.Map C8.ByteString C8.ByteString -> m ()
insertUpdate con tbl i dat = scope "insertUpdate" $ do
    [P.Only b] <- liftIO $ P.query con
        (fromString $ "select count(*) > 0 from " ++ tableName tbl ++ " where id = ?") (P.Only i)
    if b
        then update con tbl i dat
        else insert con tbl dat

update :: MonadLog m => P.Connection -> TableDesc -> C8.ByteString -> M.Map C8.ByteString C8.ByteString -> m ()
update con tbl i dat = scope "update" $ do
    liftIO $ P.execute con
        (fromString $ "update " ++ tableName tbl ++ " set " ++ intercalate ", " setters) actualDats
    return ()
    where
        (actualNames, actualDats) = removeNonColumns tbl dat
        setters = map (++ " = ?") actualNames

insert :: MonadLog m => P.Connection -> TableDesc -> M.Map C8.ByteString C8.ByteString -> m ()
insert con tbl dat = scope "insert" $ do
    liftIO $ P.execute con
        (fromString $ "insert into " ++ tableName tbl ++ " values (" ++ intercalate ", " (replicate (length actualDats) "?") ++ ")") actualDats
    return ()
    where
        (_, actualDats) = removeNonColumns tbl dat

-- | Remove invalid fields
removeNonColumns :: TableDesc -> M.Map C8.ByteString C8.ByteString -> ([String], [Text])
removeNonColumns tbl = unzip . filter ((`elem` fields) . fst) . map (C8.unpack *** T.decodeUtf8) . M.toList where
    fields = map columnName $ tableFields tbl

-- | Load model description and convert to table
loadTableDesc :: ModelGroups -> String -> String -> IO TableDesc
loadTableDesc g base f = do
    d <- loadDesc base f
    ungrouped <- either error return $ ungroup g d
    either error return $ retype ungrouped

-- | Create query for table
createTableQuery :: TableDesc -> String
createTableQuery (TableDesc nm _ inhs flds) = concat $ creating ++ inherits where
    creating = [
        "create table if not exists ", nm,
        " (", intercalate ", " (map (\(TableColumn n t) -> n ++ " " ++ t) flds), ")"]
    inherits = if null inhs then [] else [" inherits (", intercalate ", " inhs, ")"]

-- | Alter table add column queries for each field of table
extendTableQueries :: TableDesc -> [String]
extendTableQueries (TableDesc nm _ inhs flds) = map extendColumn flds where
    extendColumn :: TableColumn -> String
    extendColumn (TableColumn n t) = concat ["alter table ", nm, " add column ", n, " ", t]

-- | Load model description
loadDesc :: String -> String -> IO ModelDesc
loadDesc base f = do
    cts <- B.readFile (base </> f)
    maybe (error "Can't load") return $ decode cts

-- | Load group fields
loadGroups :: String -> IO ModelGroups
loadGroups field_groups = do
    cts <- B.readFile field_groups
    maybe (error "Can't load") return $ decode cts

-- | Unfold groups, adding fields from groups to model (with underscored prefix)
ungroup :: ModelGroups -> ModelDesc -> Either String ModelDesc
ungroup (ModelGroups g) (ModelDesc nm fs) = (ModelDesc nm . concat) <$> mapM ungroup' fs where
    ungroup' (ModelField fname ftype Nothing) = return [ModelField fname ftype Nothing]
    ungroup' (ModelField fname ftype (Just gname)) =
        maybe
            (Left $ "Can't find group " ++ gname)
            (return . map appends)
            (M.lookup gname g)
        where
            appends (ModelField fname' ftype' _) = ModelField (fname ++ "_" ++ fname') ftype' Nothing

-- | Convert model description to table description with silly type converting
retype :: ModelDesc -> Either String TableDesc
retype (ModelDesc nm fs) = TableDesc (nm ++ "tbl") nm [] <$> mapM retype' fs where
    retype' (ModelField fname Nothing _) = return $ TableColumn fname "text"
    retype' (ModelField fname (Just ftype) _) = TableColumn fname <$> maybe unknown Right (lookup ftype retypes) where
        unknown = Left $ "Unknown type: " ++ ftype
    retypes = [
        ("datetime", "timestamp"),
        ("dictionary", "text"),
        ("phone", "text"),
        ("checkbox", "bool"),
        ("date", "timestamp"),
        ("picker", "text"),
        ("map", "text"),
        ("textarea", "text"),
        ("reference", "text"),
        ("files", "text"),
        ("json", "text"),
        ("partnerTable", "text"),
        ("statictext", "text")]

-- | Add id column
addId :: TableDesc -> TableDesc
addId = addColumn "id" "integer"

-- | Add column
addColumn :: String -> String -> TableDesc -> TableDesc
addColumn nm tp (TableDesc n mdl h fs) = TableDesc n mdl h $ (TableColumn nm tp) : fs

-- | Services
services :: [String]
services = [
    "deliverCar",
    "deliverParts",
    "hotel",
    "information",
    "rent",
    "sober",
    "taxi",
    "tech",
    "towage",
    "transportation",
    "ken",
    "bank",
    "tickets",
    "continue",
    "deliverClient",
    "averageCommissioner",
    "insurance"]

-- | Make service table and inherit services from it
mergeServices :: [TableDesc] -> [TableDesc]
mergeServices tbls = srvBase : (srvsInherited ++ notSrvs) where
    srvBase = addColumn "type" "text" $ TableDesc "servicetbl" "service" [] srvBaseFields
    srvsInherited = map inherit srvs
    inherit (TableDesc nm mdl inhs fs) = TableDesc nm mdl ("servicetbl" : inhs) $ fs \\ srvBaseFields

    (srvs, notSrvs) = partition ((`elem` (map addTbl services)) . tableName) tbls
    srvBaseFields = foldr1 intersect $ map tableFields srvs

    addTbl nm = nm ++ "tbl"

-- | Find table for model
tableByModel :: (Eq s, IsString s) => s -> [TableDesc] -> Maybe TableDesc
tableByModel name = find ((== name) . fromString . tableModel)

-- | WORKAROUND: Explicitly add type value for data in service-derived model
addType :: TableDesc -> M.Map C8.ByteString C8.ByteString -> M.Map C8.ByteString C8.ByteString
addType (TableDesc _ mdl _ _) dat = if mdl `elem` services then M.insert "type" (fromString mdl) dat else dat
