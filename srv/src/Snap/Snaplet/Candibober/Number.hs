{-# LANGUAGE Rank2Types #-}

-- | Generic number checkers.

module Snap.Snaplet.Candibober.Number
    ( -- * Checker combinators
      compareNumber
    )

where

import qualified Data.ByteString.Char8 as B

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Redson.Snapless.Metamodel

compareNumber :: Monad m =>
                 SlotName
              -> FieldName
              -> Ordering
              -> Integer
              -- ^ Compare integer stored in field value to this number
              -> CheckBuilderMonad m Checker
compareNumber slot field ord arg =
    return $ scopedChecker slot field $
               \fv -> case B.readInteger fv of
                        Just (fn, _) -> return $ ord == compare fn arg
                        Nothing -> error $
                                   "Could not read integer from field" ++
                                  (B.unpack fv)
