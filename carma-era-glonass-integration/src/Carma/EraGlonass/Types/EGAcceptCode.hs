{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.EraGlonass.Types.EGAcceptCode
     ( EGAcceptCode (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Swagger


-- TODO clarify if another value is possible
data EGAcceptCode
   = OK
     deriving (Eq, Show, Generic, ToSchema)

instance FromJSON EGAcceptCode where
  parseJSON (String "OK") = pure OK
  parseJSON x = typeMismatch "EGAcceptCode" x

instance ToJSON EGAcceptCode where
  toJSON OK = String "OK"
