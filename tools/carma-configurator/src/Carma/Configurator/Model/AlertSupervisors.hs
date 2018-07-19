{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.Configurator.Model.AlertSupervisors
     ( AlertSupervisors (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Word (Word16)
import           Data.Text (Text)
import           Data.Aeson


data AlertSupervisors
   = AlertSupervisors
   { email_to        :: [Text]
   , email_from      :: Text
   , email_sender    :: Text
   , carma_port      :: Word16
   , carma_host      :: Text
   , default_db_name :: Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
