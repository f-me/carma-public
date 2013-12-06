
module Carma.Model.Case
       (Case(..)
       ,caseSearchPredicate
       ,buildCaseSearchQ
       ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson as Aeson

import qualified Data.Map as Map
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
    ,("contact",    fuzzy $ matchAny
                    [one Case.contact_name, one Case.contact_contactOwner])
    ,("comment",    listOf Case.comment)
    ,("address",    fuzzy $ one Case.caseAddress_address)
    ,("callTaker",  fuzzy $ one Case.callTaker)
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
      "fullCase"
        -> modifyView
          ((defaultView :: ModelView Case) {mv_title = "Кейс"})
          caseMod
      "newCase"
        -> setMainOnly
          $ modifyView
            ((defaultView :: ModelView Case) {mv_title = "Кейс"})
            caseMod
      _ -> defaultView
      where
        setMainOnly mv = mv
          {mv_fields =
             [fv{fv_meta = Map.insert "mainToo" (Aeson.Bool True) $ fv_meta fv}
             |fv <- mv_fields mv
             ]
          }

caseMod =
  [readonly callDate
  ,readonly callTaker

  ,dict comment $ (dictOpt "Wazzup")
              {dictBounded = False}
  ,dict diagnosis2 $ (dictOpt "Diagnosis2")
              {dictParent = Just $ Model.fieldName diagnosis1}
  ,dict diagnosis3 $ (dictOpt "Diagnosis3")
  ,dict diagnosis4 $ (dictOpt "Diagnosis4")

  ,dict program $ (dictOpt "casePrograms")
              { dictType    = Just "ComputedDict"
              , dictBounded = True
              , dictTgtCat  = Just "program"
              }

  ,transform "capitalize" contact_name
  ,transform "capitalize" contact_ownerName
  ,transform "uppercase"  car_vin
  ,transform "uppercase"  car_plateNum
  ,setMeta "regexp" "plateNum" car_plateNum

  ,setType "dictionary" car_vin
  ,dict car_vin $ (dictOpt "")
              {dictType = Just "VinDict"}
  ,dict car_model $ (dictOpt "CarModels")
              {dictParent = Just $ Model.fieldName car_make}
  ,dict car_seller $ (dictOpt "")
              {dictType = Just "DealersDict"}
  ,dict car_dealerTO $ (dictOpt "")
              {dictType = Just "DealersDict"}

  ,widget "radio" car_transmission
  ,widget "radio" car_engine

  ,textarea claim
  ,invisible psaExportNeeded
  ,invisible psaExported
  ]
