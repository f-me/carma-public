module Carma.NominatimMediator.Utils where

import qualified Data.Time.Format as Time


(?) :: (a -> b) -> (b -> c) -> (a -> c)
(?) = flip (.)
{-# INLINE (?) #-}
infixl 9 ?

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
{-# INLINE (<&>) #-}
infixr 5 <&>

secondInMicroseconds :: Float
secondInMicroseconds = 1000 * 1000

timeFormat :: String
timeFormat = "%Y-%m-%d %H:%M:%S"

formatTime :: Time.FormatTime t => t -> String
formatTime = Time.formatTime Time.defaultTimeLocale timeFormat
