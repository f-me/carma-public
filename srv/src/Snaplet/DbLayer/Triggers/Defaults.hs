
module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where 

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Snap
import Snap.Snaplet.Auth

import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis
import Snaplet.DbLayer.Types

import Util
import Utils.RKCCalc

-- | Populate a commit with default field values.
applyDefaults :: ModelName -> Map FieldName B.ByteString -> Handler b (DbLayer b) (Map FieldName B.ByteString)
applyDefaults model obj = do
  ct <- liftIO $ round . utcTimeToPOSIXSeconds
              <$> getCurrentTime
  cy <- liftIO $ formatTime defaultTimeLocale "%Y"
              <$> getCurrentTime
  let m  = 60 :: Int
      h  = 60 * m
      d  = 24 * h
      y  = 365 * d
      cd = Map.insert "callDate" (B.pack $ show ct) obj
  obj' <- case model of
    "partner" -> return
              $ Map.insertWith (flip const) "isActive" "0"
              $ Map.insertWith (flip const) "isDealer" "0"
              $ Map.insertWith (flip const) "isMobile" "0"
              $ obj
    "case" -> return cd
    "call" -> do
          Just u <- with auth currentUser
          let login = T.encodeUtf8 $ userLogin u
          return $ Map.insert "callTaker" login cd
    "cost_serviceTarifOption" -> return $ Map.insert "count" "1" obj
    "contract" ->
      -- Store user id in owner field if it's not present
      case Map.lookup "owner" obj of
        Nothing -> do
          Just u <- with auth currentUser
          let Just (UserId uid) = userId u
          return $ Map.insert "owner" (T.encodeUtf8 uid) obj
        _ -> return obj

    "taxi" -> do
      let parentId = obj Map.! "parentId"
      Right caseAddr <- Redis.runRedisDB redis
                $ Redis.hget parentId "caseAddress_address"
      return $ case caseAddr of
        Just addr -> Map.insert "taxiFrom_address" addr obj
        Nothing   -> obj

    "vin"  -> return $ Map.fromList
              [ ("validFrom"  , B.pack $ show ct)
              , ("validUntil" , B.pack $ show $ ct + y)
              , ("makeYear"   , B.pack cy)
              ]
    _ -> return obj

  dict <- gets rkcDict
  kase <- Redis.runRedisDB redis $ Redis.hgetall $
          fromMaybe "" $ Map.lookup "parentId" obj
  let kase' = either (\_ -> Map.empty) Map.fromList kase

  obj'' <- if model `elem` services
      then do
        return $ Map.union obj' $ Map.fromList
          [("times_expectedServiceStart",   B.pack $ show $ ct + h)
          ,("times_factServiceStart",       B.pack $ show $ ct + h)
          ,("times_expectedServiceEnd",     B.pack $ show $ ct + 2*h)
          ,("times_expectedServiceClosure", B.pack $ show $ ct + 12*h)
          ,("times_factServiceClosure",     B.pack $ show $ ct + 12*h)
          ,("times_expectedDealerInfo",     B.pack $ show $ ct + 7*d)
          ,("times_factDealerInfo",         B.pack $ show $ ct + 7*d)
          ,("times_expectedDispatch",       B.pack $ show $ ct + 10 * m)
          ,("createTime",                   B.pack $ show $ ct)
          ,("marginalCost",                 setSrvMCost model obj' kase' dict)
          ,("urgentService",                "notUrgent")
          ]
      else return obj'

  obj''' <- do
    case model of
      "cost_serviceTarifOption" -> do
        o <- liftM (Map.union obj'') (pricesFromOpt obj'')
        let srvId = fromMaybe "" $ Map.lookup "parentId" o
        srv <- Redis.runRedisDB redis $ Redis.hgetall srvId
        case Map.fromList <$> srv of
          Left _  -> return o
          Right s -> return $ fromMaybe o $ do
            ptype <- getCostField s
            price <- lookupNE ptype   o >>= mbreadDouble
            count <- lookupNE "count" o >>= mbreadDouble
            return $ Map.union o $ Map.fromList
              [ ("price", printBPrice price)
              , ("cost",  printBPrice $ price*count)
              ]

      _ -> return obj''

  return $ Map.union (Map.insert "ctime" (B.pack $ show ct) obj''')
         $ Map.findWithDefault Map.empty model defaults

services :: [B.ByteString]
services =
  ["deliverCar"
  ,"deliverParts"
  ,"hotel"
  ,"information"
  ,"rent"
  ,"sober"
  ,"taxi"
  ,"tech"
  ,"tech1"
  ,"towage"
  ,"transportation"
  ,"ken"
  ,"bank"
  ,"tickets"
  ,"continue"
  ,"deliverClient"
  ,"averageCommissioner"
  ,"insurance"
  ,"consultation"
  ]

serviceDefaults :: Map FieldName B.ByteString
serviceDefaults = Map.fromList
  [("status", "creating")
  ,("payType", "ruamc")
  ,("warrantyCase", "0")
  ,("overcosted", "0")
  ,("falseCall", "none")
  ]


defaults :: Map ModelName Object
defaults = Map.fromList
  [("case", Map.fromList
    [("caseStatus", "s0")
    ,("callerOwner", "1")
    ,("services", "")
    ,("actions", "")
    ])
  ,("towage", Map.union serviceDefaults $ Map.fromList 
    [("towerType", "evac")
    ,("towType", "dealer")
    ,("vandalism", "0")
    ,("accident", "0")
    ,("wheelsUnblocked", "w0")
    ,("canNeutral", "0")
    ,("towingPointPresent", "0")
    ,("manipulatorPossible", "0")
    ,("suburbanMilage", "0")
    ])
  ,("deliverCar", serviceDefaults)
  ,("deliverParts", serviceDefaults)
  ,("hotel", Map.union serviceDefaults $ Map.fromList
    [("hotelProvidedFor", "0")
    ])
  ,("information", serviceDefaults)
  ,("tech1", serviceDefaults)
  ,("consultation", serviceDefaults)
  ,("rent", Map.union serviceDefaults $ Map.fromList
    [("providedFor", "0")
    ])
  ,("sober", Map.union serviceDefaults $ Map.fromList
    [("multidrive", "0")
    ])
  ,("taxi", serviceDefaults)
  ,("tech", Map.union serviceDefaults $ Map.fromList
    [("suburbanMilage", "0")
    ])
  ,("transportation", Map.union serviceDefaults $ Map.fromList
    [("transportType", "continue")
    ])
  ,("programPermissions", Map.fromList
    [ ("showTable", "1")
    , ("showForm",  "1")
    ])
  ,("contract", Map.fromList
    [("isActive", "1")
    ])
  ]

-- | Copy price1 and price2 from tarifOption to new cost_serviceTarifOption
pricesFromOpt :: Map B.ByteString B.ByteString -> Handler b (DbLayer b) (Map B.ByteString B.ByteString)
pricesFromOpt obj = do
  case Map.lookup "tarifOptionId" obj of
    Nothing -> return Map.empty
    Just id -> do
      o <- Redis.runRedisDB redis $ Redis.hgetall id
      case o of
        Left _     -> return Map.empty
        Right opt' -> do
          let lp p = fromMaybe "" $ Map.lookup p $ Map.fromList opt'
          return $ Map.fromList
            [ ("price1", lp "price1")
            , ("price2", lp "price2")
            ]
