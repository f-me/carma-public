{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Utils where

import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           System.IO (stderr, hPrint)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import           Data.Maybe

import qualified Data.Enumerator as E (tryIO, Iteratee())
import           Data.CSV.Enumerator as CSV
import           Database.Redis as Redis


-- FIXME: use `create` form snaplet-redson 
redisSet c keyPrefix val = runRedis c $ do
  Right keyInt <- incr $ B.concat ["global:", keyPrefix, ":id"]
  let keyStr = B.concat [keyPrefix,":",B.pack $ show keyInt]
  redisSetWithKey' keyStr val


redisSetVin c val
  = runRedis c
  $ mapM_ (\k -> redisSetWithKey' (mkKey k) val) vins
  where
    -- Внезапно оказалось, что у машины может быть два VINа.
    -- В качестве быстрого решения, пусть записи о машине дублируются.
    vins = B.words $ val M.! "vin"
    mkKey k = B.concat ["vin:", k]


redisSetWithKey' key val = do
  res <- hmset key $ M.toList val
  case res of
    Left err -> liftIO $ print err
    _ -> return ()


loadXFile store fName keyMap
  = foldCSVFile fName defCSVSettings run ()
  >>= hPrint stderr -- Just to force evaluation
  where
    run :: () -> ParsedRow MapRow -> E.Iteratee B.ByteString IO ()
    run _ (ParsedRow (Just r)) = E.tryIO
      $ either (B.hPutStrLn stderr) store
      $ remap keyMap r
    run _ _ = return ()


-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList <$> foldl f (Right []) keyMap
  where
    f err@(Left _) _ = err
    f (Right res) (key',f)
      = f m >>= \val -> return
        (if B.null val then res else ((key',val):res))

