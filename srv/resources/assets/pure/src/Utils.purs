module Utils
     ( module StoreConnect
     , module ReactComponent
     , (<.>)
     , addClassName
     ) where

import Prelude

import Utils.StoreConnect as StoreConnect
import Utils.ReactComponent as ReactComponent


addClassName :: String -> String -> String
addClassName a b = a <> " " <> b

infixr 5 addClassName as <.>
