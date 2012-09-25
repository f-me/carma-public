{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
import Control.Monad.IO.Class
import Control.Monad
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
  forM_ (filter (not . B.isPrefixOf "partner:name") ps) $ \k -> do
    Right p <- hgetall k
    let p' = M.fromList p
    case M.lookup "services" $ M.fromList p of
      Nothing -> return ()
      Just ss -> forM_ (B.split ',' ss) $ \srvId -> do
        srv <- hgetall srvId
        hmset srvId [("priority1",
                      fromMaybe "" $ lookupNE "priority1" p')
                    ,("priority2",
                      fromMaybe "" $ lookupNE "priority2" p')
                    ,("priority3",
                      fromMaybe "" $ lookupNE "priority3" p')
                    ]
        liftIO $ print $ "move priorities from: " ++ show k ++ " to " ++ show srvId
        case srv of
          Left _  -> return ()
          Right s -> do
            let tname = lookupNE "tarifName" p'
                p1    = lookupNE "price1"    p'
                p2    = lookupNE "price2"    p'
                pr1   = lookupNE "priority1" p'
                pr2   = lookupNE "priority2" p'
                pr3   = lookupNE "priority3" p'
            case all (== Nothing) [tname, p1,p2] of
              True  -> return ()
              False -> do
                Right id <- incr $ modelIdKey "tarifOption"
                let idStr = B.pack $ show id
                    tkey  = objKey "tarifOption" idStr
                hmset tkey [("id"        , idStr)
                           ,("parentId"  , srvId)
                           ,("optionName", fromMaybe "" tname)
                           ,("price1"    , fromMaybe "" p1   )
                           ,("price2"    , fromMaybe "" p2   )
                           ]
                hmset srvId [("tarifOptions", tkey)]
                liftIO $ print $ "set " ++ show srvId ++ " tarifOptions: " ++ show tkey
                return ()

updCasePartner = do
  Right ps <- keys "partner:*"
  plst <- forM (filter (not . B.isPrefixOf "partner:name") ps) $ \k -> do
    Right p <- hgetall k
    return (fromMaybe "" $ M.lookup "name" $ M.fromList p, M.fromList p)
  -- make map partner -> partnerid
  let pmap = M.fromList plst
  Right casesIds <- keys "case:*"
  cases <- map M.fromList <$> rights <$> mapM hgetall casesIds
  -- thru cases
  forM_ cases $ \k -> do
    let ss = B.split ',' $ fromMaybe "" $ M.lookup "services" k
    -- thru services
    forM_ ss $ \sId -> do
      Right srv <- hgetall sId
      let srvm = M.fromList srv
      case flip M.lookup pmap =<< M.lookup "contractor_partner" srvm of
        Nothing  -> return ()
        Just p   -> do
          let partnerId = objKey "partner" $ fromJust $ M.lookup "id" p
          hmset sId [( "contractor_partnerId"
                     , partnerId
                     )]
          liftIO $ print $ "setting " ++ show sId ++ " contractor_partnerId " ++ show partnerId
          case return . B.split ',' =<< M.lookup "services" p of
            Nothing -> return ()
            Just ps -> do
              let srvName = head $ B.split ':' sId
              pservices <- map M.fromList <$> rights <$> mapM hgetall ps
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
