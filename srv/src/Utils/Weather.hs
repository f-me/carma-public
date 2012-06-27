{-# LANGUAGE TemplateHaskell #-}
module Utils.Weather () where

import Data.Aeson.TH
import WeatherApi

$(deriveToJSON id ''Weather)