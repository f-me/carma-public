
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.View where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson as Aeson
import Data.Typeable

import Data.Model
import Data.Model.Sql


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

data View m = View
  { modelName :: Text
  , title     :: Text
  , fields    :: [FieldView m]
  }


instance ToJSON (View m) where
  toJSON v = object
    [ "name"      .= modelName v
    , "title"     .= title v
    , "fields"    .= fields v
    , "canCreate" .= True
    , "canRead"   .= True
    , "canUpdate" .= True
    , "canDelete" .= True
    ]


defaultView :: forall m . Model m => View m
defaultView = View
  { modelName = T.pack $ tableName (undefined :: m)
  , title = ""
  , fields = [defaultFieldView f | f <- modelFields :: [FieldDesc m]]
  }


defaultFieldView :: FieldDesc m -> FieldView m
defaultFieldView f = FieldView
  { name      = fd_name f
  , fieldType = translateFieldType $ fd_type f
  , meta      = Map.fromList
    $  [("label", Aeson.String $ fd_desc f)]
    ++ case words $ show $ fd_type f of
      ["Ident", model] ->
        [("dictionaryName", Aeson.String $ T.pack model)
        ,("dictionaryType", "ModelDict")
        ,("bounded", Aeson.Bool True)
        ]
      _ -> []
  , canWrite  = True
  }



translateFieldType tr
  | show (typeRepTyCon tr) == "Maybe"
    = translateFieldType (head $ typeRepArgs tr)
  | otherwise = case show tr of
    "Int"  -> "int"
    "Text" -> "text"
    "Bool" -> "Bool"
    t | "Vector" `isPrefixOf` t -> "multi-dict"
      | "Ident " `isPrefixOf` t -> "dictionary" -- dictionary ?
      | otherwise -> error $ "translateFieldType: unkonwn type " ++ t
