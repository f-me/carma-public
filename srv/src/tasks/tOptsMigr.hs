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
    updPartnerOpts
    updCasePartner

updPartnerOpts = do
  Right ps <- keys "partner:*"
  -- 1272 - cases in old format
  let partners = filter (not . B.isPrefixOf "partner:name") ps
  forM_ partners $ \k -> do
    lp $ "migrating: " ++ show k
    Right partner <- g k
    let services = lookupNE "services" partner
    liftIO $ print services
    case return . ('[' ==) . B.head  =<< services of
      Nothing    -> do
        liftIO $ print "nothing in services"
        return ()
      Just True  -> do
        liftIO $ print "bad in services"
        return ()
      Just False -> do
        let sids = B.split ',' $ fromMaybe "" $ services
        ss <- grefs sids
        let cmpSrv a b = (M.lookup "serviceName" a) == (M.lookup "serviceName" b)
        -- group services by their name
        let groups = groupBy cmpSrv ss
        forM_ groups $ \sgrp -> do
          let optIds = foldl union [] $ map (mkids "tarifOptions") sgrp
          opts    <- grefs optIds
          newopts <- catMaybes <$> mapM (mkOption opts) sgrp
          let newids = map (fl "id") newopts
          -- add new options to redis
          mapM_ (\o -> hmset (fl "id" o) $ M.toList $ M.delete "id" o) newopts
          let optIds' = B.intercalate "," $ concat [optIds, newids]
          let curS  = head sgrp
          lp $ "setting to: " ++ show (fl "id" curS) ++
            " options: " ++ show optIds'
          hmset (fl "id" curS) [("tarifOptions",  optIds')]
        let (newSrvs, delSrvs) = foldl (\(a,b) (c,d) -> (a ++ c, b ++ d))
                                   ([],[]) $ map (splitAt 1) groups
        lp $ "newSrvs: " ++ show (toIds newSrvs)
        lp $ "delSrvs: " ++ show (toIds delSrvs)
        hmset k [("services", toIds newSrvs)
                ,("delSrvs",  toIds delSrvs)
                ]
        return ()

mkOption opts srv = do
  let srvId = fl "id" srv
      tname = lookupNE "tarifName" srv
      p1    = lookupNE "price1"    srv
      p2    = lookupNE "price2"    srv
  case all (== Nothing) [tname, p1,p2] of
    True -> return Nothing
    False -> do
      Right id <- incr $ modelIdKey "tarifOption"
      let idStr = B.pack $ show id
          tkey  = objKey "tarifOption" idStr
          o     = M.fromList [("id"        , tkey)
                             ,("parentId"  , srvId)
                             ,("optionName", fromMaybe "" tname)
                             ,("price1"    , fromMaybe "" p1   )
                             ,("price2"    , fromMaybe "" p2   )
                             ]
      -- check that we don't have this option already
      if (any (opteq o) opts)
        then return Nothing
        else return $ Just o

opteq a b = p1 a == p1 b && p2 a == p2 b && n a == n b
  where
    p1 = ml "price1"
    p2 = ml "price2"
    n  = ml "optionName"
toIds l = B.intercalate "," $ map (fl "id") l
fl f = fromJust . M.lookup f
ml f m = fromMaybe "" $ M.lookup f m
mkids f m = filter (/= "") $ B.split ',' $ ml f m
g      id = (M.fromList <$>) <$> hgetall id
grefs ids = rights <$> mapM hgetall' ids
hgetall' id = do
  a <- hgetall id
  case a of
    Left r  -> return $ Left r
    Right r -> return $ Right $ M.insert "id" id $ M.fromList r

updCasePartner = do
  Right ps <- keys "partner:*"
  plst <- forM (filter (not . B.isPrefixOf "partner:name") ps) $ \k -> do
    Right p <- hgetall' k
    return (fromMaybe "" $ M.lookup "name" p, p)
  -- make map partner -> partnerid
  let pmap = M.fromList plst
  Right casesIds <- keys "case:*"
  cases <- rights <$> mapM hgetall' casesIds
  -- thru cases
  forM_ cases $ \k -> do
    let ss = B.split ',' $ fromMaybe "" $ M.lookup "services" k
    lp $ "services for " ++ show k ++ " :: " ++ show ss
    -- thru services
    forM_ ss $ \sId -> do
      Right srv <- hgetall' sId
      case flip M.lookup pmap =<< M.lookup "contractor_partner" srv of
        Nothing  -> return ()
        Just p   -> do
          let partnerId = fromJust $ M.lookup "id" p
          hmset sId [( "contractor_partnerId"
                     , partnerId
                     )]
          liftIO $ print $ "setting " ++ show sId ++ " contractor_partnerId " ++ show partnerId
          case return . B.split ',' =<< M.lookup "services" p of
            Nothing -> return ()
            Just ps -> do
              let srvName = head $ B.split ':' sId
              pservices <- rights <$> mapM hgetall' ps
              let samesrv = find (\s -> (fromMaybe "" $ M.lookup "serviceName" s) == srvName) pservices
              case samesrv of
                Nothing -> return ()
                Just v  -> do
                  let perc = fromMaybe "" $ M.lookup "falseCallPercent" v
                  hmset sId [("falseCallPercent", perc)]
                  liftIO $ print $ "set " ++ show sId ++ " falseCallPercent " ++ show perc
                  return ()


-- | Like Map.lookup but treat Just "" as Nothing
lookupNE :: Ord k => k -> Map k B.ByteString -> Maybe B.ByteString
lookupNE key obj = M.lookup key obj >>= lp
  where lp "" = Nothing
        lp v  = return v

lp = liftIO . print