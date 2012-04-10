{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Vin.Utils where

import           Control.Applicative
import           Control.Exception (Exception, try)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           System.IO (stderr)

import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit
import           Database.Redis as R

import           Data.Xlsx.Parser hiding (MapRow)


redisSetVin :: R.Connection -> MapRow ByteString -> IO ()
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


loadXlsxFile store fInput fError keyMap = do
    x <- xlsx fInput
    try $ runResourceT
           $  sheetRows x 1
           $= CL.map (remap keyMap . toByteStringRow)
           $= storeCorrect store
           $= fromCSV defCSVSettings
           -- $= encode utf8
           $$ sinkFile fError


storeCorrect :: MonadResource m
             => (R.Connection -> MapRow ByteString -> IO ())
             -> Conduit (Bool, MapRow ByteString) m (MapRow ByteString)
storeCorrect store = conduitIO
    (R.connect R.defaultConnectInfo)
    (\conn -> runRedis conn quit >> return ())
    (\conn (isCorrect,m) -> do
       if isCorrect
         then do
           liftIO $ store conn m
           return $ IOProducing []
         else
           return $ IOProducing [m]
    )
    (const $ return [])


toByteStringRow :: MapRow Text -> MapRow ByteString
toByteStringRow m = M.map T.encodeUtf8 m'
  where
    m' = M.mapKeys T.encodeUtf8 m


-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList <$> foldl f (True,[]) keyMap
  where
    f res@(a,acc) (key',f)
      = case f m of
          Left e    -> (False, ((key',e):acc))
          Right val -> if B.null val
                         then res
                         else (a, ((key',val):acc))


showMap = B.unlines . map showKV . M.toList
    where
      showKV (k,v) = B.concat ["\t", k, ": ", v]
