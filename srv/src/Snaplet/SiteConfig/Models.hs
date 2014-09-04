module Snaplet.SiteConfig.Models
    ( Field(..)
    , Model(..)
    , loadModels
    )

where

import Control.Applicative
import Control.Monad (filterM)
import Data.Aeson as Aeson
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M

import System.EasyFile hiding (Permissions)

----------------------------------------------------------------------
import Snaplet.SiteConfig.Types



-- | Field permissions property.
data Permissions = Roles [Text]
                 | Everyone
                 | Nobody
                 deriving (Show)


-- | Map of field annotations which are transparently handled by
-- server without any logic.
type FieldMeta = Map FieldName Aeson.Value


-- | Form field object.
data Field = Field { name           :: FieldName
                   , fieldType      :: Text
                   , groupName      :: Maybe Text
                   , meta           :: Maybe FieldMeta
                   , canWrite       :: Bool
                   , sortingOrder   :: Int
                   } deriving (Show)

-- | A list of properties to be applied to named fields.
data Application = Application { targets    :: [FieldName]
                               , apMeta     :: FieldMeta
                               } deriving (Show)


-- | Model describes fields and permissions.
--
-- Models are built from JSON definitions (using FromJSON instance for
-- Model) with further group splicing ('spliceGroups'), applications
-- ('doApplications')
data Model = Model { modelName      :: ModelName
                   , title          :: Text
                   , fields         :: [Field]
                   , applications   :: [Application]
                   , _canCreateM    :: Permissions
                   , _canReadM      :: Permissions
                   , _canUpdateM    :: Permissions
                   , _canDeleteM    :: Permissions
                   } deriving (Show)

-- | Used when field type is not specified in model description.
defaultFieldType :: Text
defaultFieldType = "text"


instance FromJSON Model where
    parseJSON (Object v) = Model          <$>
        v .:  "name"                      <*>
        v .:  "title"                     <*>
        v .:  "fields"                    <*>
        v .:? "applications" .!= []       <*>
        v .:? "canCreate"    .!= Nobody   <*>
        v .:? "canRead"      .!= Nobody   <*>
        v .:? "canUpdate"    .!= Nobody   <*>
        v .:? "canDelete"    .!= Nobody
    parseJSON _          = error "Could not parse model description"


instance ToJSON Model where
    toJSON mdl = object
      [ "name"       .= modelName mdl
      , "title"      .= title mdl
      , "fields"     .= fields mdl
      , "canCreate"  .= _canCreateM mdl
      , "canRead"    .= _canReadM mdl
      , "canUpdate"  .= _canUpdateM mdl
      , "canDelete"  .= _canDeleteM mdl
      ]


instance FromJSON Permissions where
    parseJSON (Bool True)  = return Everyone
    parseJSON (Bool False) = return Nobody
    parseJSON v@(Array _)  = Roles <$> parseJSON v
    parseJSON _            = error "Could not parse permissions"

instance ToJSON Permissions where
    toJSON Everyone  = Bool True
    toJSON Nobody    = Bool False
    toJSON (Roles r) = toJSON r


instance FromJSON Field where
    parseJSON (Object v) = Field        <$>
      v .:  "name"                      <*>
      v .:? "type" .!= defaultFieldType <*>
      v .:? "groupName"                 <*>
      v .:? "meta"                      <*>
      pure True                         <*>
      pure 0
    parseJSON _          = error "Could not parse field properties"

instance ToJSON Field where
    toJSON f = object
      [ "name"          .= name f
      , "type"          .= fieldType f
      , "groupName"     .= groupName f
      , "canWrite"      .= canWrite f
      , "meta"          .= meta f
      ]


instance FromJSON Application where
    parseJSON (Object v) = Application  <$>
      v .: "targets"  <*>
      v .: "meta"
    parseJSON _          = error "Could not parse application entry"


-- | Perform all applications in model.
doApplications :: Model -> Model
doApplications model
    = model{fields = foldl' processField (fields model) (applications model)}
    where
        -- Update values in old meta with those specified in
        -- application meta
        mergeFieldsMeta :: FieldMeta -> Field -> Field
        mergeFieldsMeta patchMeta original = original
          {meta = Just $ M.foldlWithKey'
            (\o k v -> M.insert k v o)
            (fromMaybe M.empty $ meta original)
            patchMeta
          }

        -- Try to perform application for fields in list.
        processField :: [Field] -> Application -> [Field]
        processField (f:fs) ap = newF : processField fs ap
            where
                newF = if name f `elem` targets ap
                       then mergeFieldsMeta (apMeta ap) f
                       else f
        processField [] _ = []


parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filename = Aeson.decode <$> LB.readFile filename


-- | Load model from specified location, performing group splicing,
-- applications and filling index cache.
loadModel :: FilePath
          -- ^ Path to model definition file
          -> IO (Maybe Model)
loadModel modelFile
    =  (fmap $ doApplications)
    <$> parseFile modelFile


-- | Build metamodel name from its file path.
pathToModelName :: FilePath -> Text
pathToModelName filepath = T.pack $ takeBaseName filepath


-- | Read all models from directory to a map.
--
-- TODO: Perhaps rely on special directory file which explicitly lists
-- all models.
loadModels :: FilePath -- ^ Models directory
           -> IO (Map ModelName Model)
loadModels cfgDir =
      do
        let directory = cfgDir </> "models"
        dirEntries <- getDirectoryContents directory
        -- Leave out non-files
        mdlFiles <- filterM doesFileExist
                 (map (directory </>) dirEntries)
        mdls <- mapM (\m -> do
                        mres <- loadModel m
                        return $! case mres of
                          Just mdl -> mdl
                          Nothing -> error $ "Could not parse " ++ m
                     ) mdlFiles
        -- Splice groups & cache indices for served models
        return $ M.fromList $
               zip (map pathToModelName mdlFiles) mdls
