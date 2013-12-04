{-|

Proxy model for old programs dictionary.

-}

module Carma.Model.OldProgram where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types ()

data OldProgram = OldProgram
  { ident                 :: PK Int OldProgram ""
  , label                 :: F Text            "label"  ""
  , value                 :: F Text            "value"  ""
  , active                :: F Bool            "active" ""
  } deriving Typeable

instance Model OldProgram where
  type TableName OldProgram = "programtbl"
  modelInfo = mkModelInfo OldProgram ident
  modelView _ = defaultView
