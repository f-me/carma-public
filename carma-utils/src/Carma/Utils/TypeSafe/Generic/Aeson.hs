{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module Carma.Utils.TypeSafe.Generic.Aeson
     ( addConstructorTag
     , addConstructorTag'
     , removeConstructorTag

     , getFieldValue
     , getFieldValue'
     , getConstructorFieldValue
     , getConstructorFieldValue'
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import           Data.Text (Text)
import           Text.InterpolatedString.QM
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson
import           Data.Aeson.Types (parseEither)

import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record


-- | Helps to build custom @FromJSON@ instances.
--
-- For example when you have different set of required fields in raw JSON data
-- depending on some field. So you're looking at that field first and then
-- decide which constructor to use by adding a "tag" field to raw JSON data to
-- give that extended raw JSON to generic @FromJSON@ implementation. It could be
-- a separation between successful and failure cases (two constructors).
--
-- Also this function is type-level protected, so if you passed a correct
-- proxied type with constructors, which one of them you're adding here, you
-- can't compile until you set correct name of one.
addConstructorTag
  :: forall t typeRep constructor
  .  ( typeRep ~ Rep t
     , ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(t, constructor)
  -> Object
  -> Object

addConstructorTag Proxy =
  addConstructorTag' (Proxy :: Proxy '(typeRep, constructor))

-- | Alternative version of "addConstructorTag'" which presumes that provided
-- type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
addConstructorTag'
  :: ( ConstructorName typeRep constructor ~ 'Just constructor
     , KnownSymbol constructor
     )
  => Proxy '(typeRep, constructor)
  -> Object
  -> Object

addConstructorTag' p@Proxy = HM.insert "tag" $ String $ constructorName' p

-- | Helps to build custom @ToJSON@ instances avoiding injecting custom
-- implementation artefacts to the result output.
removeConstructorTag :: Object -> Object
removeConstructorTag = HM.delete "tag"


-- | Helps to obtain a value from JSON @Object@ by a field key.
--
-- It returnes already parsed value from JSON @Value@,
-- that value's type is determinted by @Generic@ @Rep@.
getFieldValue
  :: forall t typeRep typeName field value
  .  ( typeRep ~ Rep t
     , TypeName typeRep ~ typeName
     , FieldName typeRep field ~ 'Just field
     , FieldType typeRep field ~ 'Just value
     , KnownSymbol typeName
     , KnownSymbol field
     , FromJSON value
     )
  => Proxy '(t, field)
  -> Object
  -> Either String value

getFieldValue Proxy = getFieldValue' (Proxy :: Proxy '(typeRep, field))

-- | Alternative version of "getFieldValue" which presumes that provided type is
-- already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
getFieldValue'
  :: forall typeRep typeName field value
  .  ( TypeName typeRep ~ typeName
     , FieldName typeRep field ~ 'Just field
     , FieldType typeRep field ~ 'Just value
     , KnownSymbol typeName
     , KnownSymbol field
     , FromJSON value
     )
  => Proxy '(typeRep, field)
  -> Object
  -> Either String value

getFieldValue' p@Proxy obj = go where
  typeName'' = typeName' (Proxy :: Proxy typeRep)
  fieldName'' = fieldName' p

  go = case HM.lookup fieldName'' obj of
    Just x  -> parseEither parseJSON x
    Nothing -> Left [qms|
      Value by field name "{fieldName''}"
      of "{typeName'' :: Text}" type not found.
    |]


-- | Helps to obtain a value from JSON @Object@ by a field key.
--
-- With specific constructor constraint.
--
-- It returnes already parsed value from JSON @Value@,
-- that value's type is determinted by @Generic@ @Rep@.
getConstructorFieldValue
  :: forall t typeRep constructor field value
  .  ( typeRep ~ Rep t
     , ConstructorName typeRep constructor ~ 'Just constructor
     , ConstructorFieldName typeRep constructor field ~ 'Just field
     , ConstructorFieldType typeRep constructor field ~ 'Just value
     , KnownSymbol constructor
     , KnownSymbol field
     , FromJSON value
     )
  => Proxy '(t, constructor, field)
  -> Object
  -> Either String value

getConstructorFieldValue Proxy =
  getConstructorFieldValue' (Proxy :: Proxy '(typeRep, constructor, field))

-- | Alternative version of "getConstructorFieldValue" which presumes that
-- provided type is already a @(Rep ofSomething)@.
--
-- Could be useful in cases when @Rep ofType@ is modified.
getConstructorFieldValue'
  :: forall typeRep constructor field value
  .  ( ConstructorName typeRep constructor ~ 'Just constructor
     , ConstructorFieldName typeRep constructor field ~ 'Just field
     , ConstructorFieldType typeRep constructor field ~ 'Just value
     , KnownSymbol constructor
     , KnownSymbol field
     , FromJSON value
     )
  => Proxy '(typeRep, constructor, field)
  -> Object
  -> Either String value

getConstructorFieldValue' p@Proxy obj = go where
  constructorName'' = constructorName' (Proxy :: Proxy '(typeRep, constructor))
  fieldName'' = constructorFieldName' p

  go = case HM.lookup fieldName'' obj of
    Just x  -> parseEither parseJSON x
    Nothing -> Left [qms|
      Value by field of "{constructorName'' :: Text}" constructor
      with name "{fieldName''}" not found.
    |]
