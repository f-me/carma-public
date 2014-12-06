module Snaplet.SiteConfig.Models
    ( Field(..)
    , Model(..)
    )

where

import Control.Applicative
import Data.Aeson as Aeson
import Data.Text (Text)
import Data.Map

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


-- | Model describes fields and permissions.
--
-- Models used to be built from JSON definitions but now are
-- constructed directly from SiteConfig snaplet.
data Model = Model { modelName      :: ModelName
                   , title          :: Text
                   , fields         :: [Field]
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
