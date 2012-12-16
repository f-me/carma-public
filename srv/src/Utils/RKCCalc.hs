module Utils.RKCCalc where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Maybe

import           Snaplet.DbLayer.Types
import           Util

setSrvMCost :: ByteString -> Object -> Object -> RKCCalc -> ByteString
setSrvMCost "towage" obj parent dict =
  calcMCost "towCost" "towCallCost" "towMileCost"
            obj parent dict

setSrvMCost "tech" obj parent dict =
  calcMCost "techCost" "techCallCost" "techMileCost"
            obj parent dict

setSrvMCost "hotel" obj parent dict = printBPrice $ providedFor * dayCost
  where
    providedFor = maybe 0 readDouble $ M.lookup "providedFor" obj
    dayCost     = rkc parent "hotelDayCost" dict

setSrvMCost "rent" obj parent dict = printBPrice $ providedFor * dayCost
  where
    providedFor = maybe 0 readDouble $ M.lookup "providedFor" obj
    dayCost     = rkc parent "rentDayCost" dict

setSrvMCost "taxi" obj parent dict = printBPrice $ rkc parent "taxiLimit" dict

setSrvMCost _        _   _      _    = "0"

calcMCost :: ByteString -> ByteString -> ByteString
             -> Object -> Object -> RKCCalc -> ByteString
calcMCost costName callName mileName obj parent dict =
  let cost'    = rkc parent costName dict
      callCost = rkc parent callName dict
      mileCost = rkc parent mileName dict
      mileage  = readDouble $ fromMaybe "0" $ M.lookup "suburbanMilage" obj
  in printBPrice $ cost' + callCost + mileage * mileCost

rkc :: Map ByteString ByteString -> ByteString -> RKCCalc -> Double
rkc parent field dict = do
  fromMaybe 0 $
    M.lookup "program" parent >>= flip M.lookup dict >>= M.lookup field
