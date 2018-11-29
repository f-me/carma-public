{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables #-}

module Carma.Utils.TypeSafe.Generic.Aeson
     ( addConstructorTag
     , addConstructorTag'
     , removeConstructorTag
     ) where

import           GHC.Generics
import           GHC.TypeLits

import           Data.Proxy
import qualified Data.HashMap.Lazy as HM
import           Data.Aeson

import           Carma.Utils.TypeSafe.Generic.DataType


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
