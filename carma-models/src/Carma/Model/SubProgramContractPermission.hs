{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Proxy model for Contract field permissions used in portal code. -}

module Carma.Model.SubProgramContractPermission
    (SubProgramContractPermission(..))

where

import Data.Model
import Data.Model.View

import Carma.Model.SubProgram.Type

instance Model SubProgramContractPermission where
  type TableName SubProgramContractPermission = "SubProgramContractPermission"
  modelInfo = mkModelInfo SubProgramContractPermission fIdent
  modelView = \case
    "" -> Just $ modifyView defaultView
                [ invisible fParent
                , required field
                , setType "dictionary" field
                , dict field $
                  (dictOpt "ContractFields"){dictBounded = True}
                ]
    _  -> Nothing
