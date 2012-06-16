
module Snaplet.DbLayer.Triggers.Defaults
  (defaults
  ) where 

import Data.Map (Map)
import qualified Data.Map as Map
import Snaplet.DbLayer.Types

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
