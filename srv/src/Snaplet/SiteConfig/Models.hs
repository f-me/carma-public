{-# LANGUAGE TemplateHaskell #-}
module Snaplet.SiteConfig.Models
  where


import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad (filterM)
import Data.Aeson as Aeson
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M

import System.EasyFile hiding (Permissions)

----------------------------------------------------------------------
import Snaplet.SiteConfig.Types



-- | Field permissions property.
data Permissions = Roles [B.ByteString]
                 | Everyone
                 | Nobody
                 deriving (Show)


-- | Map of field annotations which are transparently handled by
-- server without any logic.
type FieldMeta = Map FieldName Aeson.Value


-- | Form field object.
data Field = Field { name           :: FieldName
                   , fieldType      :: B.ByteString
                   , groupName      :: Maybe B.ByteString
                   , meta           :: Maybe FieldMeta
                   , canWrite       :: Bool
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
                   , title          :: B.ByteString
                   , fields         :: [Field]
                   , defaults       :: M.Map FieldName FieldValue
                   , applications   :: [Application]
                   , _canCreateM    :: Permissions
                   , _canReadM      :: Permissions
                   , _canUpdateM    :: Permissions
                   , _canDeleteM    :: Permissions
                   } deriving (Show)
makeLenses ''Model

-- | Used when field type is not specified in model description.
defaultFieldType :: B.ByteString
defaultFieldType = "text"


instance FromJSON Model where
    parseJSON (Object v) = Model          <$>
        v .:  "name"                      <*>
        v .:  "title"                     <*>
        v .:  "fields"                    <*>
        v .:? "defaults"     .!= M.empty  <*>
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
      , "defaults"   .= defaults mdl
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
      pure False
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


-- | A named group of fields.
type Groups = M.Map B.ByteString [Field]


-- | Build new name `f_gK` for every field of group `g` to which field
-- `f` is spliced into.
groupFieldName :: FieldName
               -- ^ Name of field which is spliced into group
               -> FieldName
               -- ^ Name of group field
               -> FieldName
groupFieldName parent field = B.concat [parent, "_", field]


-- | Replace all model fields having `groupName` annotation with
-- actual group fields.
spliceGroups :: Groups -> Model -> Model
spliceGroups groups model =
    let
        mergeMetas f gf = Just $ M.union
                          (fromMaybe M.empty $ meta f)
                          (fromMaybe M.empty $ meta gf)
        updateNames f = fromMaybe [f] $ do
            n <- groupName f
            grp <- M.lookup n groups
            return $ map (\gf -> gf{ groupName = Just n
                                   , name = groupFieldName (name f) (name gf)
                                   , meta = mergeMetas f gf
                                   }) grp
    in
        model{fields = concatMap updateNames $ fields model}


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


-- | Load groups from definitions file.
loadGroups :: FilePath -> IO (Maybe Groups)
loadGroups = parseFile


-- | Load model from specified location, performing group splicing,
-- applications and filling index cache.
loadModel :: FilePath
          -- ^ Path to model definition file
          -> Groups 
          -- ^ Group definitions
          -> IO (Maybe Model)
loadModel modelFile groups
    =  (fmap $ doApplications
             . spliceGroups groups)
    <$> parseFile modelFile


-- | Build metamodel name from its file path.
pathToModelName :: FilePath -> ModelName
pathToModelName filepath = B.pack $ takeBaseName filepath


-- | Read all models from directory to a map.
--
-- TODO: Perhaps rely on special directory file which explicitly lists
-- all models.
loadModels :: FilePath -- ^ Models directory
           -> IO (Map ModelName Model)
loadModels cfgDir =
      do
        let directory = cfgDir </> "models"
        let groupsFile = cfgDir </> "field-groups.json"
        dirEntries <- getDirectoryContents directory
        -- Leave out non-files
        mdlFiles <- filterM doesFileExist
                 (map (directory </>) dirEntries)
        gs <- loadGroups groupsFile
        case gs of
          Just groups -> do
                  mdls <- mapM (\m -> do
                                  mres <- loadModel m groups
                                  return $! case mres of
                                    Just mdl -> mdl
                                    Nothing -> error $ "Could not parse " ++ m
                               ) mdlFiles
                  -- Splice groups & cache indices for served models
                  return $ M.fromList $
                         zip (map pathToModelName mdlFiles) mdls
          Nothing -> error $ "Bad groups file " ++ groupsFile
