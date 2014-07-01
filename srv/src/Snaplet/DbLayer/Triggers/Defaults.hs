module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Maybe
import System.Locale (defaultTimeLocale)
import Snap
import Snap.Snaplet.Auth
import Snaplet.Auth.Class

import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis

import qualified Carma.Model.CaseStatus as CS
import qualified Carma.Model.PaymentType as PaymentType
import qualified Carma.Model.ServiceStatus as SS
import qualified Carma.Model.TowType as TowType

import Snaplet.DbLayer.Types

import Util
import Utils.RKCCalc


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
    "call" -> do
          Just u <- withAuth currentUser
          return $ Map.insert "callTaker" (userLogin u) cd
    "cost_serviceTarifOption" -> return $ Map.insert "count" "1" obj
    "contract" ->
      -- Store user id in owner field if it's not present
      case Map.lookup "owner" obj of
        Nothing -> do
          Just u <- withAuth currentUser
          let Just (UserId uid) = userId u
          return $ Map.insert "owner" uid obj
        _ -> return obj

    "taxi" -> do
      let parentId = T.encodeUtf8 $ obj Map.! "parentId"
      Right caseAddr <- Redis.runRedisDB redis
                $ Redis.hget parentId "caseAddress_address"
      return $ case T.decodeUtf8 <$> caseAddr of
        Just addr -> Map.insert "taxiFrom_address" addr obj
        Nothing   -> obj

    "vin"  -> return $ Map.fromList
              [ ("validFrom"  , T.pack $ show ct)
              , ("validUntil" , T.pack $ show $ ct + y)
              , ("makeYear"   , T.pack cy)
              ]
    _ -> return obj

  dict <- gets rkcDict
  kase <- Redis.runRedisDB redis $ Redis.hgetall $
          T.encodeUtf8 $ fromMaybe "" $ Map.lookup "parentId" obj
  let kase' = either (\_ -> Map.empty) trObj kase

  obj'' <- if model `elem` services
      then do
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
          ,("marginalCost",                 setSrvMCost model obj' kase' dict)
          ,("urgentService",                "notUrgent")
          ]
      else return obj'

  obj''' <- do
    case model of
      "cost_serviceTarifOption" -> do
        o <- liftM (Map.union obj'') (pricesFromOpt obj'')
        let srvId = T.encodeUtf8 $ fromMaybe "" $ Map.lookup "parentId" o
        srv <- Redis.runRedisDB redis $ Redis.hgetall srvId
        case trObj <$> srv of
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

  return $ Map.union (Map.insert "ctime" (T.pack $ show ct) obj''')
         $ Map.findWithDefault Map.empty model defaults


trObj :: [(ByteString, ByteString)] -> Object
trObj = Map.map T.decodeUtf8 . Map.mapKeys T.decodeUtf8 . Map.fromList


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
  ,("payType", identFv PaymentType.ruamc)
  ,("warrantyCase", "0")
  ,("overcosted", "0")
  ,("falseCall", "none")
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

-- | Copy price1 and price2 from tarifOption to new cost_serviceTarifOption
pricesFromOpt :: Object -> Handler b (DbLayer b) Object
pricesFromOpt obj = do
  case Map.lookup "tarifOptionId" obj of
    Nothing -> return Map.empty
    Just id -> do
      o <- Redis.runRedisDB redis $ Redis.hgetall (T.encodeUtf8 id)
      case trObj <$> o of
        Left _     -> return Map.empty
        Right opt' -> do
          let lp p = fromMaybe "" $ Map.lookup p opt'
          return $ Map.fromList
            [ ("price1", lp "price1")
            , ("price2", lp "price2")
            ]
