{-# LANGUAGE OverloadedStrings #-}

module Carma.EraGlonass.Types.RouteActionResponse
     ( RouteActionResponse (..)
     ) where

import qualified Data.HashMap.Lazy as HM
import           Data.Text (Text)
import           Data.Aeson


data RouteActionResponse
   = RouteActionResponseSuccess { message :: Text, additionalData :: Object }
   | RouteActionResponseError   { message :: Text, additionalData :: Object }
     deriving (Show, Eq)

instance ToJSON RouteActionResponse where
  toJSON (RouteActionResponseSuccess msg additional)
    = Object $ HM.fromList $
    [ ("status",          String "success")
    , ("message",         String msg)
    , ("additional_data", Object additional)
    ]

  toJSON (RouteActionResponseError msg additional)
    = Object $ HM.fromList $
    [ ("status",          String "error")
    , ("message",         String msg)
    , ("additional_data", Object additional)
    ]
