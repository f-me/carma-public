
module Carma.Model.Case
       (Case(..)
       ,caseSearchParams
       ) where

import Data.Text (Text)
import Data.Aeson as Aeson
import qualified Data.Map as Map

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
                    [one Case.contact_name, one Case.contact_ownerName])
    ,("comment",    listOf Case.comment)
    ,("address",    fuzzy $ one Case.caseAddress_address)
    ,("callTaker",  fuzzy $ one Case.callTaker)
    ]

instance Model Case where
  type TableName Case = "casetbl"
  modelInfo   = mkModelInfo Case Case.ident
  modelView v =
    case v of
      "search" -> modifyView (searchView caseSearchParams) $
                  [modifyByName "Case_id" (\v -> v { fv_type = "ident" })]
                  ++ caseMod ++ caseDicts
      "fullCase"
        -> modifyView
          ((defaultView :: ModelView Case) {mv_title = "Кейс"})
          $ caseMod ++ caseDicts ++ caseRo
      "newCase"
        -> setMainOnly
          $ modifyView
            ((defaultView :: ModelView Case) {mv_title = "Кейс"})
            $ caseMod ++ caseDicts ++ caseRo
      _ -> defaultView
      where
        setMainOnly mv = mv
          {mv_fields =
             [fv{fv_meta = Map.insert "mainToo" (Aeson.Bool True) $ fv_meta fv}
             |fv <- mv_fields mv
             ]
          }

caseDicts = [
   dict comment $ (dictOpt "Wazzup")
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

  ,setType "dictionary" car_vin
  ,dict car_vin $ (dictOpt "")
              {dictType = Just "VinDict"}
  ,dict car_model $ (dictOpt "CarModels")
              {dictParent = Just $ Model.fieldName car_make}
  ,dict car_seller $ (dictOpt "")
              {dictType = Just "DealersDict"}
  ,dict car_dealerTO $ (dictOpt "")
              {dictType = Just "DealersDict"}
  ]

caseRo = [
   readonly callDate
  ,readonly callTaker
  ]

caseMod = [
   transform "capitalize" contact_name
  ,transform "capitalize" contact_ownerName
  ,transform "capitalize" cardNumber_cardOwner
  ,transform "uppercase"  car_vin
  ,transform "uppercase"  car_plateNum
  ,setMeta "regexp" "plateNum" car_plateNum

  ,widget "radio" car_transmission
  ,widget "radio" car_engine

  ,textarea claim
  ,invisible comments
  ,invisible services
  ,invisible actions
  ]
  ++ mapWidget caseAddress_address caseAddress_coords caseAddress_map
  ++ [setMeta "cityField" (Aeson.String $ Model.fieldName city) caseAddress_map]
