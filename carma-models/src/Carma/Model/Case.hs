module Carma.Model.Case
       (Case(..)
       ,caseSearchParams
       ) where

import Data.Text (Text)
import qualified Data.Text as T
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
    ,("comment",    fuzzy $ one Case.comment)
    ,("address",    fuzzy $ one Case.caseAddress_address)
    ,("callTaker",  fuzzy $ one Case.callTaker)
    ,("files",      refMExist Case.files)
    ]

instance Model Case where
  type TableName Case = "casetbl"
  modelInfo   = mkModelInfo Case Case.ident
  modelView v =
    case v of
      "search" -> modifyView (searchView caseSearchParams) $
                  [modifyByName "Case_id" (\v -> v { fv_type = "ident" })
                  ,modifyByName "files"   (\v -> v { fv_type = "dictionary" })
                  ,dict files $ (dictOpt "ExistDict")
                              { dictType    = Just "ComputedDict"
                              , dictBounded = True
                              }
                  ]
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
      -- fullCase view, but with legacy (lowercase) model name to
      -- force client use untyped CRUD; id field is stripped out
      -- (workaround for bug #1530)
      "oldCRUD"
        -> modifyView (stripId $ mv{mv_modelName = "case"}) caseOldCRUDHacks
           where mv = (modelView "fullCase") :: ModelView Case
      _ -> defaultView
      where
        setMainOnly mv = mv
          {mv_fields =
             [fv{fv_meta = Map.insert "mainOnly" (Aeson.Bool True) $ fv_meta fv}
             |fv <- mv_fields mv
             ,not $ "caseAddress" `T.isPrefixOf` fv_name fv
             ]
          }

caseDicts = [
   dict comment $ (dictOpt "Wazzup")
              {dictBounded = False}
  ,dict diagnosis1 $ (dictOpt "Diagnosis1")
  ,dict diagnosis2 $ (dictOpt "Diagnosis2")
              {dictParent = Just $ Model.fieldName diagnosis1}
  ,dict diagnosis3 $ (dictOpt "Diagnosis3")
  ,dict diagnosis4 $ (dictOpt "Diagnosis4")
  ,setType "dictionary" car_vin
  ,dict car_vin $ (dictOpt "")
              {dictType = Just "VinDict"}
  ,dict car_model $ (dictOpt "CarModels")
              {dictParent = Just $ Model.fieldName car_make, dictBounded = True}
  ,dict car_seller $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}
  ,dict car_dealerTO $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}
  ]

-- Mark several new-style dictionaries to use dictionaryStringify,
-- wrap integers in strings to be compatible to old CRUD:
caseOldCRUDHacks =
    [ setMeta "dictionaryStringify" (Aeson.Bool True) car_class
    , setMeta "dictionaryStringify" (Aeson.Bool True) car_engine
    , setMeta "dictionaryStringify" (Aeson.Bool True) car_transmission
    , setType "text" car_mileage
    , setType "text" car_makeYear
    ]

caseRo = [
   readonly callDate
  ,readonly callTaker
  ,readonly psaExported
  ]

caseMod = [
   transform "capitalize" contact_name
  ,transform "capitalize" contact_ownerName
  ,transform "uppercase"  car_vin
  ,transform "uppercase"  car_plateNum
  ,setMeta "regexp" "plateNum" car_plateNum
  ,setMeta "regexp" "vin" car_vin
  ,setMeta "regexp" "year" car_makeYear

  ,setMeta "invisible" (Aeson.Bool True) contract

  ,widget "inline-uploader" files
  ,setMeta "reference-widget" "files" files

  ,setMeta "visibility" (Aeson.Bool True) callDate

  ,setType "datetime" repair

  ,required city
  ,required comment
  ,required diagnosis1
  ,required diagnosis2
  ,required program
  ,required car_vin
  ,required car_make
  ,required car_model
  ,required car_seller
  ,required car_plateNum
  ,required car_color
  ,required car_buyDate
  ,required car_dealerTO
  ,required car_mileage
  ,required car_transmission
  ,required vinChecked
  ,required caseStatus

  ,setMeta "cityField" "city" caseAddress_coords

  ,setMeta "regexp" "email" contact_email
  ,setMeta "regexp" "email" contact_ownerEmail
  ,setMeta "regexp" "date" car_buyDate

  ,mainToo contact_contactOwner
  ,mainToo car_plateNum

  -- TODO Move to caseDicts
  ,setMeta "dictionaryType" "ModelDict" program
  ,setMeta "dictionaryStringify" (Aeson.Bool True) program
  ,setMeta "dictionaryParent" "program" subprogram
  ,setMeta "dictionaryStringify" (Aeson.Bool True) subprogram

  ,widget "radio" car_transmission
  ,widget "radio" car_engine

  ,textarea dealerCause
  ,textarea claim
  ,invisible comments
  ,invisible actions

  ,infoText "comment" comment
  ,infoText "system" diagnosis1
  ,infoText "detail" diagnosis2
  ,infoText "diagnosis3" diagnosis3
  ,infoText "recomendation" diagnosis4
  ,infoText "owner" contact_contactOwner
  ,infoText "program" program

  ,infoText "platenum" car_plateNum
  ,infoText "vinChecked" vinChecked
  ,infoText "city" city
  ,infoText "caseAddress" caseAddress_address
  ,infoText "coords" caseAddress_coords
  ,infoText "temperature" temperature
  ,infoText "dealerCause" dealerCause
  ,infoText "claim" claim
  ]
  ++ mapWidget caseAddress_address caseAddress_coords caseAddress_map
  ++ [setMeta "cityField" (Aeson.String $ Model.fieldName city) caseAddress_map]
