{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
import Database.Redis
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           Data.Map (Map)
import Data.Maybe
import Control.Applicative
import Data.Either
import Data.List

objKey :: B.ByteString -> B.ByteString -> B.ByteString
objKey model objId = B.concat [model, ":", objId]

modelIdKey :: B.ByteString -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]

main = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    Right ps <- keys "partner:*"
    forM_ ps $ \k -> do
      Right p <- fmap M.fromList <$> hgetall k
      case lookupNE "services" p of
        Nothing -> return ()
        Just s  -> do
          when ('[' == B.head s) $ do
            lp $ "bad service: " ++ show k ++ "services: " ++ show s
            hdel k ["services"]
            return ()

-- | Like Map.lookup but treat Just "" as Nothing
lookupNE :: Ord k => k -> Map k B.ByteString -> Maybe B.ByteString
lookupNE key obj = M.lookup key obj >>= lp
  where lp "" = Nothing
        lp v  = return v

lp = liftIO . print