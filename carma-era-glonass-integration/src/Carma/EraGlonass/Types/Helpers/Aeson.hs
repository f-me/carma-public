{-# LANGUAGE ExplicitNamespaces, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds #-}

-- | Common helpers for @FromJSON@ and @ToJSON@ instances.
module Carma.EraGlonass.Types.Helpers.Aeson
     ( extrudeObject
     , extrudeObject'

     , preParseOptionalListFields
     , preParseOptionalListFields'
     , preParseOptionalListFieldsRep
     , preParseOptionalListFieldsRep'

     , parseJSONWithOptionalListFields
     , parseJSONWithOptionalListFields'
     ) where

import           GHC.Generics (Generic (Rep))
import           GHC.TypeLits (Nat, Symbol, KnownSymbol, type (<=))

import           Data.Proxy
import           Data.Text (Text)
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson
import           Data.Aeson.Types (Parser, typeMismatch)

import           Control.Monad ((>=>), foldM)

import           Carma.Utils.TypeSafe.TypeFamilies (Length)
import           Carma.Utils.TypeSafe.Generic.DataType
import           Carma.Utils.TypeSafe.Generic.Record.Operations.GetFieldNamesByType
import           Carma.Utils.TypeSafe.Serialize


-- | Extrudes @Object@ from @Value@ or fails to parse (if it's not an @Object@).
extrudeObject
  :: forall (type' :: *) (typeRep :: * -> *)
   . (typeRep ~ Rep type', KnownSymbol (TypeName typeRep))
  => Proxy type'
  -> Value
  -> Parser Object

extrudeObject Proxy = extrudeObject' (Proxy :: Proxy typeRep)

-- | Version of "extrudeObject" which works with type representation instead of
--   original type (type representation may be modified on type-level).
extrudeObject'
  :: forall (typeRep :: * -> *). (KnownSymbol (TypeName typeRep))
  => Proxy typeRep
  -> Value
  -> Parser Object

extrudeObject' Proxy (Object obj) = pure obj
extrudeObject' Proxy src = typeMismatch (typeName' (Proxy :: Proxy typeRep)) src



-- | Version of "preParseOptionalListFieldsRep" but works on original
--   type, its representation instead of just provided representation itself.
preParseOptionalListFields
  :: forall (type'       :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( typeRep ~ Rep type'
   , KnownSymbol (TypeName typeRep)
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(type', constructor, f a)
  -> Value
  -> Parser Object

preParseOptionalListFields Proxy =
  preParseOptionalListFieldsRep (Proxy :: Proxy '(typeRep, constructor, f a))

-- | Version of "preParseOptionalListFieldsRep'" but works on original
--   type, its representation instead of just provided representation itself.
preParseOptionalListFields'
  :: forall (type'       :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (n           :: Nat)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( typeRep ~ Rep type'
   , KnownSymbol (TypeName typeRep)
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(type', constructor, f a, n)
  -> Value
  -> Parser Object

preParseOptionalListFields' Proxy =
  preParseOptionalListFieldsRep'
    (Proxy :: Proxy '(typeRep, constructor, f a, n))

-- | This helps to interpret field which is not set or @Null@ as an empty array.
preParseOptionalListFieldsRep
  :: forall (typeRep     :: * -> *)
            (constructor :: Symbol)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( KnownSymbol (TypeName typeRep)
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(typeRep, constructor, f a)
  -> Value
  -> Parser Object

preParseOptionalListFieldsRep Proxy = go where
  go src@(Object obj) = foldM (reducer $ mismatch src) obj $ keys obj
  go src              = typeMismatch typeName'' src

  typeName'' = typeName' (Proxy :: Proxy typeRep)
  mismatch = typeMismatch typeName''
  keys obj = [k | k <- HM.keys obj, k `elem` optionalListFields]

  optionalListFields :: [Text]
  optionalListFields =
    getFieldNamesOfConstructorByType'
      (Proxy :: Proxy '(typeRep, constructor, fieldType))

  -- | Interpreting empty strings as @Nothing@.
  reducer mismatch' acc k = x where
    emptyListPlug = HM.insert k $ Array mempty
    x = case HM.lookup k acc of
             Nothing        -> pure $ emptyListPlug acc
             Just Null      -> pure $ emptyListPlug acc
             Just (Array _) -> pure acc
             Just _         -> mismatch'

-- | Version of "parseOptionalListFieldsRep" with verification
--   of how many such fields are supposed to be presented.
preParseOptionalListFieldsRep'
  :: forall (typeRep     :: * -> *)
            (constructor :: Symbol)
            (n           :: Nat)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( KnownSymbol (TypeName typeRep)
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(typeRep, constructor, f a, n)
  -> Value
  -> Parser Object

preParseOptionalListFieldsRep' Proxy =
  preParseOptionalListFieldsRep (Proxy :: Proxy '(typeRep, constructor, f a))


-- | An implementation of "parseJSON" which just handles optional list fields
--   (interprets not set field or @null@ as empty list).
parseJSONWithOptionalListFields
  :: forall (t           :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( Generic t
   , typeRep ~ Rep t
   , KnownSymbol (TypeName typeRep)
   , GFromJSON Zero typeRep
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   )
  => Proxy '(t, constructor, f a)
  -> Value
  -> Parser t

parseJSONWithOptionalListFields p@Proxy =
  preParseOptionalListFields p >=> genericParseJSON defaultOptions . Object

-- | Version of "parseJSONWithOptionalListFields" with verification
--   of how many such fields are supposed to be presented.
parseJSONWithOptionalListFields'
  :: forall (t           :: *)
            (typeRep     :: * -> *)
            (constructor :: Symbol)
            (n           :: Nat)
            (fieldType   :: *)
            (fieldNames  :: [Symbol])
            (f           :: * -> *)
            (a           :: *)
   .
   ( Generic t
   , typeRep ~ Rep t
   , KnownSymbol (TypeName typeRep)
   , GFromJSON Zero typeRep
   , Applicative f
   , fieldType ~ f a
   , Monoid fieldType
   , fieldNames ~ GetFieldNamesOfConstructorByType typeRep constructor fieldType
   , SerializableListOfKnownSymbols fieldNames
   , 1 <= Length fieldNames
   , n ~ Length fieldNames
   )
  => Proxy '(t, constructor, f a, n)
  -> Value
  -> Parser t

parseJSONWithOptionalListFields' p@Proxy =
  preParseOptionalListFields' p >=> genericParseJSON defaultOptions . Object
