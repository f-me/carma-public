{-# LANGUAGE OverloadedStrings #-}

{-|

Expense types, codes and servicing costs.

These should not fall into wrong hands.

TODO Load sensitive data from external files.

-}


module Carma.SAGAI.Codes
    ( ExpenseType(..)
    , CodeRow(..)
    , codesData
    , rentCostsPSA
    , rentCosts
    )

where

import Data.ByteString as BS
import Data.Map as M

import Carma.HTTP


data ExpenseType = Dossier
                 | FalseCall
                 | PhoneServ
                 | Charge
                 | Condition
                 | Starter
                 | Towage
                 | RepTowage
                 | Rent
                   deriving (Eq, Ord, Show)


-- | Data for particular type of expense.
data CodeRow = CodeRow { cost             :: Double
                       , impCode          :: BS.ByteString
                       , serviceImpCode   :: BS.ByteString
                       , causeCode        :: BS.ByteString
                       , defCode          :: BS.ByteString
                       }


-- | List of costs and I/C/D codes for all programs and expenses.
codesData :: M.Map (FieldValue, ExpenseType) CodeRow
codesData = M.fromList
    [ (("citroen", Dossier),     CodeRow 1148    "DV1" "DV4" "9938" "G5F")
    , (("citroen", FalseCall),   CodeRow 574     "DR1" "DR4" "9939" "296")
    , (("citroen", PhoneServ),   CodeRow 351     "DV1" "DV4" "9939" "G5F")
    , (("citroen", Charge),      CodeRow 1825    "DR1" "DR4" "996L" "446")
    , (("citroen", Condition),   CodeRow 1825    "DR1" "DR4" "996D" "446")
    , (("citroen", Starter),     CodeRow 1825    "DR1" "DR4" "996A" "446")
    , (("citroen", Towage),      CodeRow 2911    "DR1" "DR4" "9934" "G5F")
    , (("citroen", RepTowage),   CodeRow 2009    "DR1" "DR4" "9936" "G5F")
    , (("citroen", Rent),        CodeRow 0       "PV1" "PV4" "9927" "PZD")
    , (("peugeot", Dossier),     CodeRow 1354.64 "24R" "FCA" "8999" "G5D")
    , (("peugeot", FalseCall),   CodeRow 677.32  "24E" "FCA" "8990" "G5D")
    , (("peugeot", PhoneServ),   CodeRow 414.18  "24E" "FCA" "8943" "G5D")
    , (("peugeot", Charge),      CodeRow 2153.5  "24E" "FCA" "8962" "G5D")
    , (("peugeot", Condition),   CodeRow 2153.5  "24E" "FCA" "8954" "G5D")
    , (("peugeot", Starter),     CodeRow 2153.5  "24E" "FCA" "8963" "G5D")
    , (("peugeot", Towage),      CodeRow 3434.98 "24E" "FCA" "8950" "G5D")
    , (("peugeot", RepTowage),   CodeRow 2370.62 "24E" "FCA" "8983" "G5D")
    , (("peugeot", Rent),        CodeRow 0       "F6R" "FCA" "8997" "PZD")
    ]


-- | Daily costs for car rent service provided by PSA dealers.
--
-- Map key is a @(program, carClass)@ tuple.
rentCostsPSA :: M.Map (FieldValue, FieldValue) Double
rentCostsPSA = M.fromList
    [ (("citroen", "psab"),  1729)
    , (("citroen", "psam1"), 2034)
    , (("citroen", "psam2"), 2848)
    , (("peugeot", "psab"),  2040.22)
    , (("peugeot", "psam1"), 2400.12)
    , (("peugeot", "psam2"), 3360.64)
    ]


-- | Daily costs for car rent service provided by third-party dealers.
rentCosts :: M.Map (FieldValue, FieldValue) Double
rentCosts = M.fromList
    [ (("citroen", "psab"),  1441)
    , (("citroen", "psam1"), 1695)
    , (("citroen", "psam2"), 2373)
    , (("peugeot", "psab"),  1700.38)
    , (("peugeot", "psam1"), 2000.1)
    , (("peugeot", "psam2"), 2800.14)
    ]
