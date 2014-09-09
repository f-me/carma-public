{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings,
             QuasiQuotes,
             TypeOperators,
             FlexibleInstances,
             MultiParamTypeClasses,
             DeriveDataTypeable,
             RecordWildCards
 #-}

module Main where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (forM_, when)

import           Data.List ((\\), transpose)
import           Data.Text (Text, toLower, unpack, pack)
import qualified Data.Map as M
import           Data.HashMap.Strict hiding (map, null)
import           Text.Printf
import           Text.PrettyPrint.Boxes
import           Data.Typeable

import           System.Console.CmdLib
import           Prelude hiding (lookup)

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ

import           Data.Model
import           Data.Model.Types

import           Carma.Model as CM

data ModelErrors = ModelErrors { mdlName :: Text
                               , tblName :: Text
                               , missingInTable :: [Text]
                               , missingInModel :: [Text]
                               , typeMissmatch  :: [(Text, PgType, PgType)]
                               } deriving Show

q =  [sql|
      SELECT attname,
             pg_catalog.format_type(a.atttypid, a.atttypmod),
             attnotnull
      FROM pg_attribute a, pg_class c
      WHERE a.attrelid = c.oid AND c.relname = ? AND attnum > 0
      AND attname NOT LIKE '%.pg.dropped.%';
     |]

typesForModel :: Connection -> Text -> IO (HashMap Text PgType)
typesForModel conn tbl = do
  ts :: [(Text, Text, Bool)] <- query conn q $ Only tbl
  return $ foldl (\a (name, t, n) -> insert name (PgType t n) a) empty ts

chkModel :: Connection -> Text -> IO ModelErrors
chkModel conn model =
  case CM.dispatch model $ \m -> (tableName' m, modelFieldsMap' m) of
    Nothing -> error $ "Unknown model: " ++ (unpack model)
    Just (tbl, fsmap) -> do
      pgtypes <- typesForModel conn tbl
      let onlyTbl = keys pgtypes \\ (map toLower $ keys fsmap)
          emptyerrs = ModelErrors{ mdlName      = model
                                 , tblName      = tbl
                                 , missingInModel = onlyTbl
                                 , missingInTable = []
                                 , typeMissmatch  = []
                                 }
      return $ foldlWithKey' (checkField pgtypes) emptyerrs fsmap
  where
    tableName' :: forall m.Model m => m -> Text
    tableName' _ = tableName (modelInfo :: ModelInfo m)
    modelFieldsMap' :: forall m.Model m => m -> HashMap Text (FieldDesc)
    modelFieldsMap' _ = modelFieldsMap (modelInfo :: ModelInfo m)
    checkField pgtypes a@ModelErrors{..} k v =
      case lookup (toLower k) pgtypes of
        Nothing -> a{ missingInTable = k : missingInTable }
        Just t  -> case t == fd_pgType v of
          True  -> a
          False -> a{ typeMissmatch = (k, fd_pgType v, t) : typeMissmatch }

printErrors :: ModelErrors -> IO ()
printErrors (ModelErrors mdlName tblName [] [] []) =
  printf "Model: %s, table: %s is OK\n" (unpack mdlName) (unpack tblName)
printErrors me@ModelErrors{..} = do
  printf "Model: %s, table: %s have errors:\n" (unpack mdlName) (unpack tblName)
  when (not $ null missingInModel) $
    printf "Fields missing in model: %s\n" (show missingInModel)
  when (not $ null missingInTable) $
    printf "Fields missing in table: %s\n" (show missingInTable)
  when (not $ null typeMissmatch) $ do
    printf "Fields type mismatch:\n"
    let d = ["Field name", "Model type", "Actual type"] : map
            (\(fname, mdl, tbl) -> [unpack fname, show mdl, show tbl])
            typeMissmatch
    printBox $ hsep 1 left (map ( vcat left . map text) (transpose d))

data Main = Main { user :: String
                 , pass :: String
                 , db :: String
                 , model :: Maybe String
                 }
     deriving (Typeable, Data, Eq, Show)

instance Attributes Main where
     attributes _ = group "Options" [
         user  %> [ Help "PG user."
                  , ArgHelp "TEXT"
                  , Default ("postgres" :: String)
                  ],
         pass  %> [ Help "PG password."
                  , ArgHelp "TEXT"
                  , Default ("" :: String)
                  ],
         db    %> [ Help "Carma database"
                  , ArgHelp "TEXT"
                  , Default ("carma" :: String)
                  ],
         model %> [ Help "Model name accessible with dispatch"
                  , ArgHelp "TEXT"
                  , Default (Nothing:: Maybe String)
                  ]
         ]

instance RecordCommand Main where
  mode_summary _ = "Carma model types checker."

main = do
  let allowedModels = M.keys $ modelMap typeOf
  getArgs >>= executeR Main {} >>= \opts -> do
    conn <- connect defaultConnectInfo { connectUser     = user opts
                                       , connectDatabase = db opts
                                       , connectPassword = pass opts
                                       }
    case model opts of
      Nothing -> do
        mapM_ (\m -> chkModel conn m >>= printErrors >> printf "\n")
          allowedModels
      Just m  ->
        if (pack m) `elem` allowedModels
        then chkModel conn (pack m) >>= printErrors
        else printf "Unknown model, allowed are: %s\n" (show allowedModels)
