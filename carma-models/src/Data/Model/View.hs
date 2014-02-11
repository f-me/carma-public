{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.View
  (defaultView
  ,defaultFieldView
  ,modifyView
  ,stripId
  ,textarea
  ,readonly
  ,required
  ,invisible
  ,dict, dictOpt, DictOpt(..)
  ,completeWith
  ,mapWidget
  ,mainToo
  ,mainOnly
  ,transform
  ,widget
  ,setType
  ,infoText
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


-- | Strip @id@ field from a view (workaround for bug #1530).
stripId :: forall m . Model m => ModelView m -> ModelView m
stripId mv = mv{mv_fields = filter (\f -> fv_name f /= "id") $ mv_fields mv}


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


infoText
  :: SingI name => Text -> (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
infoText t = setMeta "infoText" (Aeson.String t)


readonly
  :: SingI name => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
readonly = setMeta "readonly" (Aeson.Bool True)


required
  :: SingI name => (m -> Field typ (FOpt name desc))
  -> (Text, FieldView -> FieldView) :@ m
required = setMeta "required" (Aeson.Bool True)


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


-- | Add autocompletion from a dictionary to a text field. Any value
-- is stored as-is (without converting to internal dictionary element
-- id).
--
-- Example:
--
-- > someField `completeWith` Dictionary.label
completeWith :: forall m m1 t n d t1 n1 d1.
                (Model m1, SingI n, SingI n1) =>
                (m -> F t n d)
             -> (m1 -> F t1 n1 d1)
             -- ^ Target dictionary field.
             -> (Text, FieldView -> FieldView) :@ m
completeWith fld ann =
    Wrap
    ( fieldName fld
    ,   us (setType "dictionary" fld)
      . us (setMeta "dictionaryType"
            (Aeson.String "ModelDict") fld)
      . us (setMeta "dictionaryName"
            (Aeson.String $
             Model.modelName (modelInfo :: ModelInfo m1)) fld)
      . us (setMeta "bounded" (Aeson.Bool False) fld)
      . us (setMeta "dictionaryLabel"
            (Aeson.String $ Model.fieldName ann) fld)
      -- This will force saving of labels even for in-dictionary values
      . us (setMeta "dictionaryKey"
            (Aeson.String $ Model.fieldName ann) fld)
    )
    where
      us = snd . unWrap


mapWidget
  :: (SingI n1, SingI n2, SingI n3)
  => (m -> Field pickerField (FOpt n1 d1))
  -> (m -> Field pickerField (FOpt n2 d2))
  -> (m -> Field mapField    (FOpt n3 d3))
  -> [(Text, FieldView -> FieldView) :@ m]
mapWidget addr coords mapWid =
  [Wrap (fieldName addr,  us $ setMeta "picker" "geoPicker" addr)
  ,Wrap (fieldName addr,  us
    $ setMeta "targetCoords" (Aeson.String $ fieldName coords) addr)
  ,Wrap (fieldName addr,  us
    $ setMeta "targetMap" (Aeson.String $ fieldName mapWid) addr)
  ,Wrap (fieldName coords, us $ setMeta "picker" "reverseGeoPicker" coords)
  ,Wrap (fieldName coords, us
    $ setMeta "targetAddr" (Aeson.String $ fieldName addr) coords)
  ,Wrap (fieldName coords, us
    $ setMeta "targetMap" (Aeson.String $ fieldName mapWid) coords)
  ,Wrap (fieldName mapWid, us
    $ setMeta "targetCoords" (Aeson.String $ fieldName coords) mapWid)
  ,Wrap (fieldName mapWid, us
    $ setMeta "targetAddr" (Aeson.String $ fieldName addr) mapWid)
  ]
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
