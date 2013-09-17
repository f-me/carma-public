
module Data.Model.View.Types where

import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson as Aeson


data FieldView m = FieldView
  { name      :: Text
  , fieldType :: Text
  , meta      :: Map Text Aeson.Value
  , canWrite  :: Bool
  }

instance ToJSON (FieldView m) where
  toJSON f = object
    [ "name"     .= name f
    , "type"     .= fieldType f
    , "canWrite" .= canWrite f
    , "meta"     .= meta f
    ]

data ModelView m = ModelView
  { modelName :: Text
  , title     :: Text
  , fields    :: [FieldView m]
  }

instance ToJSON (ModelView m) where
  toJSON v = object
    [ "name"      .= modelName v
    , "title"     .= title v
    , "fields"    .= fields v
    , "canCreate" .= True
    , "canRead"   .= True
    , "canUpdate" .= True
    , "canDelete" .= True
    ]
