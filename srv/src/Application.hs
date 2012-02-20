{-# LANGUAGE TemplateHaskell #-}

module Application where

import Data.Lens.Template
import Snap.Snaplet

data App = App
type AppHandler = Handler App App
makeLens ''App


