
module Snaplet.DbLayer.Triggers.Defaults
  (applyDefaults
  ) where 

import Control.Monad.IO.Class
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import Snaplet.DbLayer.Types


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
            ]
          | otherwise -> obj

  return $ Map.union obj'
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
