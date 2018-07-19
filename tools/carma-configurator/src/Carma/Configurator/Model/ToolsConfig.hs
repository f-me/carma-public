{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.Configurator.Model.ToolsConfig
     ( ToolsConfig (..)
     , module Carma.Configurator.Model.AlertSupervisors
     ) where

import           GHC.Generics (Generic)

import           Data.Aeson

import           Carma.Configurator.Model.AlertSupervisors


data ToolsConfig
   = ToolsConfig
   { alert_supervisors :: AlertSupervisors
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
