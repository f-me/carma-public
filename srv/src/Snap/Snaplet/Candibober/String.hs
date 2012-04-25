{-# LANGUAGE Rank2Types #-}

-- | Generic string checkers.

module Snap.Snaplet.Candibober.String
    ( -- * Checker combinators
      fieldInList
    , fieldContains
    , fieldEquals
    )

where

import qualified Data.ByteString as B

import Snap.Snaplet.Candibober.Types

import Snap.Snaplet.Redson.Snapless.Metamodel


------------------------------------------------------------------------------
-- | String-based field checks with argument of type @a@.
type StringCheck a = Monad m =>
                     SlotName -> FieldName -> a -> CheckBuilderMonad m Checker


------------------------------------------------------------------------------
-- | Check if field value is present in list.
fieldInList :: StringCheck [B.ByteString]
fieldInList slot field vals =
    return $ scopedChecker slot field $
               \fv -> return $ elem fv vals


------------------------------------------------------------------------------
-- | Check if field contains a substring.
fieldContains :: StringCheck B.ByteString
fieldContains slot field val =
    return $ scopedChecker slot field $
               \fv -> return $ B.null $ snd (B.breakSubstring val fv)


------------------------------------------------------------------------------
-- | Check if field contains a substring.
fieldEquals :: StringCheck B.ByteString
fieldEquals slot field val =
    return $ scopedChecker slot field $
               \fv -> return $ fv == val
