module Carma.Model.CaseSearch
  (caseSearchPredicate
  ,caseSearchView
  ) where

import Data.Text (Text)
import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Database.PostgreSQL.Simple as PG

import Data.Model
import Data.Model.View
import Data.Model.Search

import Carma.Model.Case as Case


caseSearchParams :: [(Text, [Predicate Case])]
caseSearchParams
  = [("caseId",     one (ident :: IdentF Case))
    ,("vin",        fuzzy $ one Case.car_vin)
    ,("cardNumber", fuzzy $ one Case.cardNumber_cardNumber)
    ,("plateNum",   fuzzy $ one Case.car_plateNum)
    ,("phone",      fuzzy $ matchAny
      [one Case.contact_phone1, one Case.contact_phone2
      ,one Case.contact_phone3, one Case.contact_phone4
      ,one Case.contact_ownerPhone1, one Case.contact_ownerPhone2
      ,one Case.contact_ownerPhone3, one Case.contact_ownerPhone4
      ])
    ,("program",    listOf Case.program)
    ,("city",       listOf Case.city)
    ,("carMake",    listOf Case.car_make)
    ]


caseSearchPredicate
  :: PG.Connection -> Aeson.Value -> IO (Either String Text)
caseSearchPredicate c v = case v of
  Aeson.Object o -> renderPredicate c params o
  _ -> return $ Left $ "Object expected but found: " ++ show v
  where
    params = HM.fromList caseSearchParams


caseSearchView :: ModelView Case
caseSearchView = searchView caseSearchParams
