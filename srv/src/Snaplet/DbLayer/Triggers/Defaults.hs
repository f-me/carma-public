module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Snap
import Snap.Snaplet.Auth
import Snaplet.Auth.Class

import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis

import qualified Carma.Model.CaseStatus as CS
import qualified Carma.Model.ServiceStatus as SS
import qualified Carma.Model.FalseCall as FC
import qualified Carma.Model.TowType as TowType

import Snaplet.DbLayer.Types

import Util


-- | Populate a commit with default field values.
applyDefaults :: HasAuth b
              => ModelName
              -> Object
              -> Handler b (DbLayer b) Object
applyDefaults model obj = do
  ct <- liftIO $ round . utcTimeToPOSIXSeconds
              <$> getCurrentTime
  cy <- liftIO $ formatTime defaultTimeLocale "%Y"
              <$> getCurrentTime
  let m  = 60 :: Int
      h  = 60 * m
      d  = 24 * h
      y  = 365 * d
      cd = Map.insert "callDate" (T.pack $ show ct) obj
  obj' <- case model of
    "partner" -> return
              $ Map.insertWith (flip const) "isActive" "0"
              $ Map.insertWith (flip const) "isDealer" "0"
              $ Map.insertWith (flip const) "isMobile" "0"
              $ obj
    "case" -> return cd
    "cost_serviceTarifOption" -> return $ Map.insert "count" "1" obj
    "taxi" -> do
      let parentId = T.encodeUtf8 $ obj Map.! "parentId"
      Right caseAddr <- Redis.runRedisDB redis
                $ Redis.hget parentId "caseAddress_address"
      return $ case T.decodeUtf8 <$> caseAddr of
        Just addr -> Map.insert "taxiFrom_address" addr obj
        Nothing   -> obj
    _ -> return obj

  obj'' <- if model `elem` services
      then
        return $ Map.union obj' $ Map.fromList
          [("times_expectedServiceStart",   T.pack $ show $ ct + h)
          ,("times_factServiceStart",       T.pack $ show $ ct + h)
          ,("times_expectedServiceEnd",     T.pack $ show $ ct + 2*h)
          ,("times_expectedServiceClosure", T.pack $ show $ ct + 12*h)
          ,("times_factServiceClosure",     T.pack $ show $ ct + 12*h)
          ,("times_expectedDealerInfo",     T.pack $ show $ ct + 7*d)
          ,("times_factDealerInfo",         T.pack $ show $ ct + 7*d)
          ,("times_expectedDispatch",       T.pack $ show $ ct + 10 * m)
          ,("createTime",                   T.pack $ show $ ct)
          ,("urgentService",                "notUrgent")
          ]
      else return obj'

  return $ Map.union (Map.insert "ctime" (T.pack $ show ct) obj'')
         $ Map.findWithDefault Map.empty model defaults


services :: [Text]
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

serviceDefaults :: Object
serviceDefaults = Map.fromList
  [("status", identFv SS.creating)
  ,("warrantyCase", "0")
  ,("overcosted", "0")
  ,("falseCall", identFv FC.none)
  ]


defaults :: Map ModelName Object
defaults = Map.fromList
  [("case", Map.fromList
    [("caseStatus", identFv CS.front)
    ,("callerOwner", "1")
    ,("services", "")
    ,("actions", "")
    ])
  ,("towage", Map.union serviceDefaults $ Map.fromList
    [("towerType", "evac")
    ,("towType", identFv TowType.dealer)
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
