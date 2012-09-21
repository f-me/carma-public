{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import Control.Monad.IO.Class
import Control.Monad
import Database.Redis
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

objKey :: B.ByteString -> B.ByteString -> B.ByteString
objKey model objId = B.concat [model, ":", objId]

modelIdKey :: B.ByteString -> B.ByteString
modelIdKey model = B.concat ["global:", model, ":id"]

main = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    updPartnerOpts

updPartnerOpts = do
  Right ps <- keys "partner:*"
  forM_ (filter (not . B.isPrefixOf "partner:name") ps) $ \k -> do
    Right p <- hgetall k
    let p' = M.fromList p
        tname = M.lookup "tarifName" p'
        p1    = M.lookup "price1"    p'
        p2    = M.lookup "price2"    p'
    case all (== Nothing) [tname, p1,p2] of
      True  -> trace ("allempty" ++ show k ++ show tname ++ show p1 ++ show p2) $ return ()
      False -> do
        Right id <- incr $ modelIdKey "tarifOption"
        let idStr = B.pack $ show id
            tkey  = objKey "tarifOption" idStr
        hmset tkey [("id", idStr )
                   ,("optionName", fromMaybe "" tname)
                   ,("price1"    , fromMaybe "" p1   )
                   ,("price2"    , fromMaybe "" p2   )
                   ]
        hmset k [("tarifOptions", tkey)]
        return ()
  -- trace (show $ filter (not . B.isPrefixOf "partner:name") ps) $ return ()

updCasePartner = do
  return ()