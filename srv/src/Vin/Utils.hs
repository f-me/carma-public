{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Vin.Utils where

import           Control.Applicative
import           Control.Exception (try)
import           Control.Monad.IO.Class (liftIO)
import           System.IO (stderr, hPrint)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import           Data.Maybe

import qualified Data.Enumerator as E (tryIO, Iteratee())
import           Database.Redis as Redis

import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as T

import           Data.Xlsx.Parser


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


loadXlsxFile store fName keyMap = do
    x <- xlsx fName
    try $ runResourceT $ sheetRows x 1 $$ CL.foldM run ()
  where
    run a r = either
                (\msg -> B.hPutStrLn stderr msg >> return a)
                store
              $ remap keyMap $ toByteStringRow r


toByteStringRow :: MapRow -> M.Map ByteString ByteString
toByteStringRow m = M.map T.encodeUtf8 m'
  where
    m' = M.mapKeys T.encodeUtf8 m


-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList <$> foldl f (Right []) keyMap
  where
    f err@(Left _) _ = err
    f (Right res) (key',f)
      = f m >>= \val -> return
        (if B.null val then res else ((key',val):res))


showMap = B.unlines . map showKV . M.toList
    where
      showKV (k,v) = B.concat ["\t", k, ": ", v]
