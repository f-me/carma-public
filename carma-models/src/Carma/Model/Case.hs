
module Carma.Model.Case
       (Case(..)
       ,caseSearchPredicate
       ,caseSearchView
       ,buildCaseSearchQ
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson as Aeson

import qualified Data.HashMap.Strict as HM
import Database.PostgreSQL.Simple as PG

import Data.Model
import Data.Model.View
import Data.Model.Search as S

import Carma.Model.Types()
import Carma.Model.Case.Type as Case


caseSearchParams :: [(Text, [Predicate Case])]
caseSearchParams
  = [("Case_id",     one Case.ident)
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
    ,("callDate",   interval Case.callDate)
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

instance Model Case where
  type TableName Case = "casetbl"
  modelInfo   = mkModelInfo Case Case.ident
  modelView v =
    case v of
      "search" -> caseSearchView
      _        -> defaultView

buildCaseSearchQ :: Connection -> Int -> Aeson.Value
                    -> IO (Either String Text)
buildCaseSearchQ conn limit v  = do
  ps <- caseSearchPredicate conn v
  return $ ps >>= return . wrapPredicates
  where
    wrapPredicates p = wrapJson $ T.concat [pre, p, post]
    pre  = "SELECT * FROM servicesview where "
    post = " LIMIT " `T.append ` (T.pack $ show limit)
    wrapJson t = T.concat ["SELECT row_to_json(r) FROM ( ", t ," ) r"]
