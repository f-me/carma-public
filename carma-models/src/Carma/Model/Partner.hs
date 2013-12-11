{-|

Proxy model for a subset of legacy partner model.

-}

module Carma.Model.Partner where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Types ()

data Partner = Partner
  { ident :: PK Int Partner "Партнёр"
  , label :: F Text         "name" "Название"
  , value :: F (Maybe Text) "code" "Код"
  } deriving Typeable

instance Model Partner where
  type TableName Partner = "partnertbl"
  modelInfo = mkModelInfo Partner ident
  modelView _ = defaultView
