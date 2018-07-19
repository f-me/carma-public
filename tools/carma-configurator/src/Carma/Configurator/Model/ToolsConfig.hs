{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.Configurator.Model.ToolsConfig
     ( ToolsConfig (..)
     , module Carma.Configurator.Model.AlertSupervisors
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson

import           Carma.Configurator.Model.AlertSupervisors
import           Carma.Configurator.Model.ArcVinImport


data ToolsConfig
   = ToolsConfig
   { alert_supervisors :: AlertSupervisors
   , arc_vin_import    :: ArcVinImport
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
