-- | Generic string checkers.

module Snap.Snaplet.Candibober.String
    ( -- * Checker combinators
      inListCheck
    )

where

import qualified Data.ByteString as B

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Redson.Snapless.Metamodel

------------------------------------------------------------------------------
-- | Check if field value is present in list.
inListCheck :: Monad m =>
               SlotName
            -> FieldName
            -> [B.ByteString]
            -> CheckBuilderMonad m Checker
inListCheck slot field vals =
    let
        fcheck :: FieldChecker
        fcheck fv = return $ elem fv vals
    in
      return $ scopedChecker slot field fcheck