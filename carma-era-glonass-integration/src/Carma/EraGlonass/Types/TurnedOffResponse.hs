{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Carma.EraGlonass.Types.TurnedOffResponse
     ( TurnedOffService (..)
     , TurnedOffResponse (..)
     ) where

import qualified Data.HashMap.Lazy as HM
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Aeson


data TurnedOffService = VinSynchronizer | StatusSynchronizer deriving Eq

instance Show TurnedOffService where
  show = \case
    VinSynchronizer    -> "Vin Synchronizer"
    StatusSynchronizer -> "Status Synchronizer"

data TurnedOffResponse
   = TurnedOffResponse { message :: Text, service :: TurnedOffService }
     deriving (Show, Eq)

instance ToJSON TurnedOffResponse where
  toJSON (TurnedOffResponse msg service')
    = Object $ HM.fromList $
    [ ("status",  String "error")
    , ("message", String msg)
    , ("service", String $ fromString $ show service')
    ]
