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
redisSet c modelName val = runRedis c $ do
  Right keyInt <- incr $ B.concat ["global:", modelName, ":id"]
  let keyStr = B.concat [modelName,":",B.pack $ show keyInt]
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


loadXFile store = loadXFile' () (\_ v -> store v >> return ())


loadXFile' arg store fName keyMap
  = foldCSVFile fName defCSVSettings run arg
  where
--    run :: a -> ParsedRow MapRow -> E.Iteratee B.ByteString IO a
    run a (ParsedRow (Just r)) = E.tryIO
      $ either
        (\msg -> B.hPutStrLn stderr msg >> return a)
        (store a)
      $ remap keyMap r
    run a _ = return a


-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList <$> foldl f (Right []) keyMap
  where
    f err@(Left _) _ = err
    f (Right res) (key',f)
      = f m >>= \val -> return
        (if B.null val then res else ((key',val):res))

