{-# LANGUAGE ScopedTypeVariables #-}

-- A module that helps to deal with `RequestId` of Era Glonass service
module Carma.EraGlonass.RequestId
     ( RequestId
     , newRequestId
     ) where

import           Data.Monoid ((<>))
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Digest.Pure.MD5 (md5)

import           Control.Monad.Random.Class (MonadRandom, getRandoms)

import           Carma.Monad.Clock


-- `RequestId` is a free string that looks like:
--   "c94eea91-d647-43d2-af04-109fbb53d8dc".
--
-- Looking at this example we could see it looks like it is just an MD5 hash
-- with some dashes.
--
-- It's part of Era Glonass system, so we don't know what actually it is except
-- it must be unique for each request to Era Glonass.
--
newtype RequestId = RequestId ByteString deriving (Eq, Show)


-- Builds new unique `RequestId`.
newRequestId :: (MonadRandom m, MonadClock m) => m RequestId
newRequestId = do
  (randomPart  :: [Char]) <- take 128 <$> getRandoms
  (currentTime :: [Char]) <- show <$> getCurrentTime

  pure $ RequestId
       $ Data.ByteString.Char8.pack
       $ interleaveWithDashes
       $ show $ md5
       $ Data.ByteString.Lazy.Char8.pack
       $ randomPart <> ('|' : currentTime)

  where -- Lengths of parts of hash to interleave with dashes.
        -- Last part length is omitted.
        dashesParts = [8, 4, 4, 4] :: [Int]

        -- Puts dashes to proper places of hash string
        interleaveWithDashes :: String -> String
        interleaveWithDashes x = uncurry f $ foldl reducer ("", x) dashesParts
          where f a b = a <> ('-' : b)
                reducer (prevAcc, rest) i = (newAcc, b)
                  where (a, b) = splitAt i rest
                        newAcc = if null prevAcc then a else f prevAcc a
