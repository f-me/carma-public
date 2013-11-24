
module Carma.Model.Case
       (Case(..)
       ,caseSearchPredicate
       ,buildCaseSearchQ
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson as Aeson

import qualified Data.HashMap.Strict as HM
import Database.PostgreSQL.Simple as PG

import Data.Model as Model
import Data.Model.View as View

import Carma.Model.Search as S
import Carma.Model.Types()
import Carma.Model.Case.Type as Case


caseSearchParams :: [(Text, [Predicate Case])]
caseSearchParams
  = [("Case_id",    one Case.ident)
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


instance Model Case where
  type TableName Case = "casetbl"
  modelInfo   = mkModelInfo Case Case.ident
  modelView v =
    case v of
      "search" -> modifyView (searchView caseSearchParams) $
                  [modifyByName "Case_id" (\v -> v { fv_type = "text" })]
                  ++ caseMod
      _        -> modifyView
        ((defaultView :: ModelView Case) {mv_title = "Кейс"}) caseMod

caseMod =
  [readonly callDate
  ,readonly callTaker

  ,dict comment $ dictOpt "Wazzup"
  ,dict diagnosis1 $ dictOpt "Diagnosis1"
  ,dict diagnosis2 $ (dictOpt "Diagnosis2")
              {dictParent = Just $ Model.fieldName diagnosis1}
  ,dict diagnosis3 $ (dictOpt "Diagnosis3")
              {dictParent = Just $ Model.fieldName diagnosis2}
  ,dict diagnosis4 $ (dictOpt "Diagnosis4")
              {dictParent = Just $ Model.fieldName diagnosis3}

  ,dict program $ (dictOpt "casePrograms")
              { dictType    = Just "ComputedDict"
              , dictBounded = True
              , dictTgtCat  = Just "program"
              }

  ,dict car_make $ (dictOpt "CarMake")
              {dictBounded = True}
  ,dict car_model $ (dictOpt "CarModel")
              {dictBounded = True
              ,dictParent = Just $ Model.fieldName car_make}
  ,dict car_seller (dictOpt "DealersDict")
              {dictBounded = True}
  ,dict car_dealerTO (dictOpt "DealersDict")
              {dictBounded = True}
  ,dict car_color $ (dictOpt "Colors")
  ,dict vinChecked $ (dictOpt "VINChecked")
              {dictBounded = True}
  ,dict car_contractType $ (dictOpt "ContractType")
              {dictBounded = True}

  ,dict car_transmission $ dictOpt "Transmission"
  ,widget "radio" car_transmission

  ,dict car_engine $ dictOpt "EngineType"
  ,widget "radio" car_engine

  ,dict city $ (dictOpt "DealerCities")
              {dictBounded = True}
  ,dict caseStatus $ (dictOpt "CaseStatuses")
              {dictBounded = True}
  ,invisible psaExportNeeded
  ,invisible psaExported
  ]
