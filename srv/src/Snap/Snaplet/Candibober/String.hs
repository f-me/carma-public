-- | Generic string checkers.

module Snap.Snaplet.Candibober.String
    ( -- * Checker combinators
      fieldInList
    )

where

import qualified Data.ByteString as B

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Redson.Snapless.Metamodel

------------------------------------------------------------------------------
-- | Check if field value is present in list.
fieldInList :: Monad m =>
               SlotName
            -> FieldName
            -> [B.ByteString]
            -> CheckBuilderMonad m Checker
fieldInList slot field vals =
    let
        fcheck :: FieldChecker
        fcheck fv = return $ elem fv vals
    in
      return $ scopedChecker slot field fcheck
