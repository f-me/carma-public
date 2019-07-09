{-# LANGUAGE DataKinds, TypeFamilies, InstanceSigs, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Carma.EraGlonass.Types.EGMayFailToParse
     ( EGMayFailToParse (..)
     ) where

import           GHC.Generics

import           Data.Text (Text)
import           Data.Proxy
import           Data.Aeson
import           Data.Swagger

import           Carma.Utils.TypeSafe.Generic.Record


-- | A wrapper for any request or response type which helps to handle parsing
--   failures manually.
--
-- We're storing such errors in database with original request or response body
-- which we can extract from this wrapper along with error message.
data EGMayFailToParse a
   = SuccessfullyParsed a
   | FailedToParse
   { errorMessage  :: String
   , incorrectBody :: Value
   } deriving (Eq, Show, Generic)

-- | It just returns original schema of a wrapped type.
--
-- This wrapper type is only for internal usage, no need to show it in a spec.
instance ToSchema a => ToSchema (EGMayFailToParse a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance FromJSON a => FromJSON (EGMayFailToParse a) where
  parseJSON src = pure $
    case fromJSON src of
         Success x -> SuccessfullyParsed x
         Error   e -> FailedToParse e src

instance ToJSON a => ToJSON (EGMayFailToParse a) where
  toJSON
    :: forall t c. (t ~ EGMayFailToParse a, c ~ "FailedToParse") => t -> Value

  toJSON (SuccessfullyParsed x) = toJSON x
  toJSON (FailedToParse err src) = object
    [ "status" .= ("error" :: Text)
    , constructorFieldName (Proxy :: Proxy '(t, c, "errorMessage")) .= err
    , constructorFieldName (Proxy :: Proxy '(t, c, "incorrectBody")) .= src
    ]
