{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExplicitNamespaces, KindSignatures, DataKinds #-}

module Carma.EraGlonass.Types.EGContractId
     ( type EGContractId (..)
     ) where

import           GHC.Generics (type Generic)

import           Data.Proxy (type Proxy (Proxy))
import           Data.Text (type Text)
import           Data.Aeson (type ToJSON, type FromJSON)
import           Data.Swagger (type ToSchema (declareNamedSchema))
import           Data.Configurator.Types
                   ( type Value (String)
                   , type Configured (convert)
                   )

import           Carma.EraGlonass.Types.EGIntegrationPoint


-- | An identity of a contract between CaRMa and Era Glonass.
--
-- * @EGContractId 'BindVehicles@ is given by EG to CaRMa, it's static and
--   supposed to be provided in microservice config;
--
-- * @EGContractId 'ChangeProcessingStatus@ is coming from related Era Glonass
--   participant @SubProgram@.
--
newtype EGContractId (a :: EGIntegrationPoint)
      = EGContractId Text
        deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToSchema (EGContractId a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance Configured (EGContractId a) where
  convert (String x) = Just $ EGContractId x
  convert _ = Nothing
