{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs, TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Carma.EraGlonass.Types.Helpers.NonEmptyText
     ( NonEmptyText
     , fromNonEmptyText
     , toNonEmptyText

     , preParseOptionalNonEmptyTextFields
     , preParseOptionalNonEmptyTextFields'
     , preParseOptionalNonEmptyTextFieldsRep
     , preParseOptionalNonEmptyTextFieldsRep'

     , parseJSONWithOptionalNonEmptyTextFields
     , parseJSONWithOptionalNonEmptyTextFields'
     ) where

import           GHC.Generics (Generic (Rep))
import           GHC.TypeLits (Nat, Symbol, KnownSymbol, type (<=))
import           Generics.Deriving.Eq
import           Generics.Deriving.Show

import           Data.Proxy
import           Data.Text (Text)
import           Text.Printf (PrintfArg)
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)
import           Data.Swagger
import qualified Data.HashMap.Lazy as HM

import           Control.Monad ((>=>), foldM)
import           Control.DeepSeq (NFData)

import           Carma.Utils.Operators
import           Carma.Utils.TypeSafe.Proxy
import           Carma.Utils.TypeSafe.TypeFamilies (Length)
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record.Operations.GetFieldNamesByType
import           Carma.Utils.TypeSafe.Serialize


newtype NonEmptyText
      = NonEmptyText Text
        deriving (Generic, Show, Eq, Ord, Semigroup, PrintfArg, NFData)

instance GShow NonEmptyText where gshowsPrec = showsPrec
instance GEq   NonEmptyText where geq = (==)

fromNonEmptyText :: NonEmptyText -> Text
fromNonEmptyText (NonEmptyText x) = x

-- | Returns @Nothing@ in case @Text@ is empty.
toNonEmptyText :: Text -> Maybe NonEmptyText
toNonEmptyText x | x == mempty = Nothing | otherwise = Just $ NonEmptyText x

instance ToJSON NonEmptyText where
  toJSON = String . fromNonEmptyText

instance FromJSON NonEmptyText where
  parseJSON :: forall t. t ~ NonEmptyText => Value -> Parser t
  parseJSON (String x) = go where
    go = maybe failure pure $ toNonEmptyText x
    failure = fail $ "Passed empty string for " <> typeName (Proxy :: Proxy t)
  parseJSON src = typeMismatch (typeName (Proxy :: Proxy t)) src

instance ToSchema NonEmptyText where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy Text)
      <&> \case x@NamedSchema { _namedSchemaSchema = schema' } -> x
                  { _namedSchemaSchema
                      = schema'
                      { _schemaMinProperties = Just 1
                      , _schemaDescription   = Just "Non-empty string"
                      }
                  }


-- | Version of "preParseOptionalNonEmptyTextFieldsRep" but works on original
--   type, its representation instead of just provided representation itself.
preParseOptionalNonEmptyTextFields
  :: forall (type'       :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
   .
   ( typeRep ~ Rep type'
   , KnownSymbol (TypeName typeRep)
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(type', constructor)
  -> Value
  -> Parser Object

preParseOptionalNonEmptyTextFields Proxy =
  preParseOptionalNonEmptyTextFieldsRep (Proxy :: Proxy '(typeRep, constructor))

-- | Version of "preParseOptionalNonEmptyTextFieldsRep'" but works on original
--   type, its representation instead of just provided representation itself.
preParseOptionalNonEmptyTextFields'
  :: forall (type'       :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (n           :: Nat)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
   .
   ( typeRep ~ Rep type'
   , KnownSymbol (TypeName typeRep)
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(type', constructor, n)
  -> Value
  -> Parser Object

preParseOptionalNonEmptyTextFields' Proxy =
  preParseOptionalNonEmptyTextFieldsRep'
    (Proxy :: Proxy '(typeRep, constructor, n))


-- | Extracts all optional "NonEmptyText" fields from @Generic@ type
--   representation and removes any empty strings from json @Object@ so those
--   empty string will be interpreted as @Nothing@.
preParseOptionalNonEmptyTextFieldsRep
  :: forall (typeRep     :: * -> *)
            (constructor :: Symbol)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
   .
   ( KnownSymbol (TypeName typeRep)
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(typeRep, constructor)
  -> Value
  -> Parser Object

preParseOptionalNonEmptyTextFieldsRep p@Proxy = go where
  go src@(Object obj) = foldM (reducer $ mismatch src) obj $ keys obj
  go src              = mismatch src

  typeName'' = typeName' (Proxy :: Proxy typeRep)
  mismatch = typeMismatch typeName''
  keys obj = [k | k <- HM.keys obj, k `elem` optionalNonEmptyTextFields]

  optionalNonEmptyTextFields :: [Text]
  optionalNonEmptyTextFields
    = getFieldNamesOfConstructorByType'
    $ proxyPair2Triplet p (Proxy :: Proxy fieldType)

  -- | Interpreting empty strings as @Nothing@.
  reducer mismatch' acc k =
    case HM.lookup k acc of
         Nothing -> pure acc
         Just (String x)
           | x == mempty -> pure $ HM.delete k acc
           | otherwise   -> pure acc
         _ -> mismatch'

-- | Version of "preParseOptionalNonEmptyTextFieldsRep" with verification
--   of how many such fields are supposed to be presented.
preParseOptionalNonEmptyTextFieldsRep'
  :: forall (typeRep     :: * -> *)
            (constructor :: Symbol)
            (n           :: Nat)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
   .
   ( KnownSymbol (TypeName typeRep)
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(typeRep, constructor, n)
  -> Value
  -> Parser Object

preParseOptionalNonEmptyTextFieldsRep' Proxy =
  preParseOptionalNonEmptyTextFieldsRep (Proxy :: Proxy '(typeRep, constructor))


-- | An implementation of "parseJSON" which just handles optional
--   @NonEmptyText@ fields (interprets empty strings as @Nothing@).
parseJSONWithOptionalNonEmptyTextFields
  :: forall (t          :: *)
            (typeRep    :: * -> *)
            (c          :: Symbol)
            (fieldType  :: *)
            (fieldNames :: [Symbol])
   .
   ( Generic t
   , typeRep ~ Rep t
   , KnownSymbol (TypeName typeRep)
   , GFromJSON Zero typeRep
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep c fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(t, c)
  -> Value
  -> Parser t

parseJSONWithOptionalNonEmptyTextFields p@Proxy =
  preParseOptionalNonEmptyTextFields p
    >=> genericParseJSON defaultOptions . Object

-- | Version of "parseJSONWithOptionalNonEmptyTextFields" with verification
--   of how many such fields are supposed to be presented.
parseJSONWithOptionalNonEmptyTextFields'
  :: forall (t          :: *)
            (typeRep    :: * -> *)
            (c          :: Symbol)
            (n          :: Nat)
            (fieldType  :: *)
            (fieldNames :: [Symbol])
   .
   ( Generic t
   , typeRep ~ Rep t
   , KnownSymbol (TypeName typeRep)
   , GFromJSON Zero typeRep
   , fieldType ~ Maybe NonEmptyText
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep c fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(t, c, n)
  -> Value
  -> Parser t

parseJSONWithOptionalNonEmptyTextFields' p@Proxy =
  preParseOptionalNonEmptyTextFields' p
    >=> genericParseJSON defaultOptions . Object
