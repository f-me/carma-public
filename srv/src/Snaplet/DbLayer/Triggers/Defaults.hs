
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
      obj' = case model of
        "case" -> cd
        "call" -> cd
        "vin"  -> Map.fromList
                  [ ("validFrom"  , B.pack $ show ct)
                  , ("validUntil" , B.pack $ show $ ct + y)
                  , ("makeYear"   , B.pack cy)
                  ]
        _ | model `elem` services ->
          Map.union obj $ Map.fromList
            [("times_expectedServiceStart",   B.pack $ show $ ct + h)
            ,("times_factServiceStart",       B.pack $ show $ ct + h)
            ,("times_expectedServiceEnd",     B.pack $ show $ ct + 2*h)
            ,("times_expectedServiceClosure", B.pack $ show $ ct + 12*h)
            ,("times_factServiceClosure",     B.pack $ show $ ct + 12*h)
            ,("times_expectedDealerInfo",     B.pack $ show $ ct + 7*d)
            ,("times_factDealerInfo",         B.pack $ show $ ct + 7*d)
            ,("createTime",                   B.pack $ show $ ct)
            ]
          | otherwise -> obj

  obj'' <- do
    case model of
      "cost_serviceTarifOption" -> do
        o <- liftM (Map.union obj') (pricesFromOpt obj')
        let srvId = fromMaybe "" $ Map.lookup "parentId" o
        ptype <- Redis.runRedisDB redis $ Redis.hget srvId "payType"
        case ptype of
          Left _  -> return o
          Right p -> return $ fromMaybe o $ do
            price <- p >>= selectPrice >>= flip Map.lookup o >>= mbreadDouble
            count <- lookupNE "count" o >>= mbreadDouble
            return $ Map.fromList [("price", printBPrice price)
                                  ,("cost",  printBPrice $ price*count)
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
  case (flip Map.lookup obj) "tarifOptionId" of
    Nothing -> return Map.empty
    Just id -> do
      o <- Redis.runRedisDB redis $ Redis.hgetall id
      case o of
        Left _     -> return Map.empty
        Right opt' -> do
          let p1 = lookupNE "price1" $ Map.fromList opt'
              p2 = lookupNE "price2" $ Map.fromList opt'
          case sequence [p1,p2] of
            Nothing        -> return Map.empty
            Just [p1',p2'] -> return $ Map.fromList
                              [("price1", p1'), ("price2", p2')]