{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.Configurator.Model.AlertSupervisors
     ( AlertSupervisors (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Word (Word16)
import           Data.Text (Text)
import           Data.Aeson

import           Carma.Configurator.Types


data AlertSupervisors
   = AlertSupervisors
   { email_to        :: [Email]
   , email_from      :: Email
   , email_sender    :: Email
   , carma_port      :: Word16
   , carma_host      :: Text
   , default_db_name :: Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
