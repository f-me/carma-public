{-# LANGUAGE TemplateHaskell #-}

module Application where

import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.RedisDB
import Database.Redis


data App = App
  {_redisDB :: Snaplet RedisDB
  }
  
type AppHandler = Handler App App
makeLens ''App


