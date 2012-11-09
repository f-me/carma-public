
module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where 

import Control.Monad.IO.Class
import Control.Monad (liftM)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Maybe
import System.Locale (defaultTimeLocale)
import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis
import Snaplet.DbLayer.Types

import Util

applyDefaults model obj = do
  ct <- liftIO $ round . utcTimeToPOSIXSeconds
              <$> getCurrentTime
  cy <- liftIO $ formatTime defaultTimeLocale "%Y"
              <$> getCurrentTime
  let h  = 60*60 :: Int
      d  = 24 * h
      y  = d * 365
      cd = Map.insert "callDate" (B.pack $ show ct) obj
  obj' <- case model of
    "partner" -> return
              $ Map.insert "isActive" "0"
              $ Map.insert "isDealer" "0"
              $ obj
    "case" -> return cd
    "call" -> return cd
    "cost_serviceTarifOption" -> return $ Map.insert "count" "1" obj
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
    _ -> return cd

  obj' <- if model `elem` services
      then
        return $ Map.union obj' $ Map.fromList
          [("times_expectedServiceStart",   B.pack $ show $ ct + h)
          ,("times_factServiceStart",       B.pack $ show $ ct + h)
          ,("times_expectedServiceEnd",     B.pack $ show $ ct + 2*h)
          ,("times_expectedServiceClosure", B.pack $ show $ ct + 12*h)
          ,("times_factServiceClosure",     B.pack $ show $ ct + 12*h)
          ,("times_expectedDealerInfo",     B.pack $ show $ ct + 7*d)
          ,("times_factDealerInfo",         B.pack $ show $ ct + 7*d)
          ,("createTime",                   B.pack $ show $ ct)
          ]
      else return obj'

  obj'' <- do
    case model of
      "cost_serviceTarifOption" -> do
        o <- liftM (Map.union obj') (pricesFromOpt obj')
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

      _ -> return obj'

  return $ Map.union (Map.insert "ctime" (B.pack $ show ct) obj'')
         $ Map.findWithDefault Map.empty model defaults


services =
  ["deliverCar"
  ,"deliverParts"
  ,"hotel"
  ,"information"
  ,"rent"
  ,"sober"
  ,"taxi"
  ,"tech"
  ,"towage"
  ,"transportation"
  ,"ken"
  ,"bank"
  ,"tickets"
  ,"continue"
  ,"deliverClient"
  ,"averageCommissioner"
  ,"insurance"
  ]

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
  ,("rent", Map.union serviceDefaults $ Map.fromList
    [("carProvidedFor", "0")
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
  ]

-- | Copy price1 and price2 from tarifOption to new cost_serviceTarifOption
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
