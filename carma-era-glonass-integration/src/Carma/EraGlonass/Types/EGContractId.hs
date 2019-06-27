{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Carma.EraGlonass.Types.EGContractId
     ( EGContractId (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Proxy (Proxy (Proxy))
import           Data.Text (Text)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Swagger (ToSchema (declareNamedSchema))
import           Data.Configurator.Types (Value (String), Configured (convert))


-- | An identity of contract between CaRMa and Era Glonass.
--
-- It is given by EG to CaRMa.
--
-- Used to identify CaRMa on Era Glonass side when making requests from CaRMa to
-- Era Glonass services. When Era Glonass makes requests to CaRMa it already
-- knows to which partner it makes requests to since it's associated with
-- CaRMa's service IP address.
--
-- This id is constant after you run the service, it supposed to be defined in
-- the service's configuration file.
newtype EGContractId = EGContractId Text
        deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToSchema EGContractId where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance Configured EGContractId where
  convert (String x) = Just $ EGContractId x
  convert _ = Nothing
