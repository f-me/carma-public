
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
import Data.Model.View.Types


defaultView :: forall m . Model m => ModelView m
defaultView = ModelView
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
    ++ case fd_name f of
      "id" -> [("invisible", Aeson.Bool True)]
      _ -> []
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
