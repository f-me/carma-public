module App.Store.Actions
     ( AppAction (..)
     ) where

import Router (Location)
import App.Store.DiagTree.Actions (DiagTreeAction)


data AppAction
  = Navigate Location
  | DiagTree DiagTreeAction
