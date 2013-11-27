
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.View
  (defaultView
  ,defaultFieldView
  ,modifyView
  ,textarea
  ,readonly
  ,invisible
  ,dict, dictOpt, DictOpt(..)
  ,mainToo
  ,mainOnly
  ,transform
  ,widget
  ,setType
  ,setMeta
  ,modifyByName
  -- from Data.Model.View.Types
  ,FieldView(..)
  ,ModelView(..)
  ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Aeson as Aeson

import GHC.TypeLits

import Data.Model.Types
import Data.Model as Model


defaultView :: forall m . Model m => ModelView m
defaultView
  = modifyView mv
    [Wrap
      (primKeyName mi, \v -> v
        {fv_type = "ident"
        ,fv_canWrite  = False
        ,fv_meta = Map.insert "invisible" (Aeson.Bool True) $ fv_meta v
      })]
  where
    mi = modelInfo :: ModelInfo m
    mv = ModelView
      { mv_modelName = Model.modelName mi
      , mv_title = ""
      , mv_fields = map fd_view $ modelFields mi
      }


modifyView
  :: ModelView m -> [(Text, FieldView -> FieldView) :@ m]
  -> ModelView m
modifyView mv@(ModelView{mv_fields}) fns
  = mv {mv_fields = map ((fMap' Map.!) . fv_name) mv_fields}
  where
    fMap = Map.fromList [(fv_name f, f) | f <- mv_fields]
    fMap' = foldl' tr fMap fns
    tr m (Wrap (nm, fn)) = Map.adjust fn nm m


-- field modificators

setType
  :: SingI name => Text -> (m -> Field t (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
setType typ fld = Wrap
  (fieldName fld, \v -> v {fv_type = typ})

textarea
  :: SingI name => (m -> Field t (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
textarea fld = Wrap
  (fieldName fld
  ,\v -> v {fv_type = "textarea"})


setMeta
  :: SingI name
  => Text -> Aeson.Value
  -> (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
setMeta key val fld = Wrap
  (fieldName fld
  ,\v -> v {fv_meta = Map.insert key val $ fv_meta v}
  )


readonly
  :: SingI name => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
readonly = setMeta "readonly" (Aeson.Bool True)

invisible
  :: SingI name => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
invisible = setMeta "invisible" (Aeson.Bool True)


data DictOpt = DictOpt
  {dictName    :: Text
  ,dictType    :: Maybe Text
  ,dictBounded :: Bool
  ,dictTgtCat  :: Maybe Text
  ,dictParent  :: Maybe Text
  }

dictOpt :: Text -> DictOpt
dictOpt nm = DictOpt
  {dictName    = nm
  ,dictType    = Nothing
  ,dictBounded = False
  ,dictTgtCat  = Nothing
  ,dictParent  = Nothing
  }

dict
  :: SingI name
  => (m -> Field typ (FOpt name desc)) -- FIXME: typ ~ Ident xx
  -> DictOpt
  -> (Text, FieldView -> FieldView) :@ m
dict fld (DictOpt{..}) = Wrap
  (fieldName fld
  ,us (setMeta "dictionaryName" (Aeson.String dictName) fld)
    . maybe id
      (\v -> us $ setMeta "dictionaryType" (Aeson.String v) fld)
      dictType
    . us (setMeta "bounded" (Aeson.Bool dictBounded) fld)
    . maybe id
      (\v -> us $ setMeta "targetCategory" (Aeson.String v) fld)
      dictTgtCat
    . maybe id
      (\v -> us $ setMeta "dictionaryParent" (Aeson.String v) fld)
      dictParent
  )
  where
    us = snd . unWrap

widget
  :: SingI name
  => Text -> (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
widget nm = setMeta "widget" (Aeson.String nm)

mainToo
  :: SingI name
  => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
mainToo = setMeta "mainToo" (Aeson.Bool True)

mainOnly
  :: SingI name
  => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
mainOnly = setMeta "mainOnly" (Aeson.Bool True)


transform
  :: SingI name
  => Text -> (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
transform tr = setMeta "transform" (Aeson.String tr)

modifyByName :: Text -> (FieldView -> FieldView)
             -> (Text, FieldView -> FieldView) :@ m
modifyByName name fn = Wrap (name, fn)
