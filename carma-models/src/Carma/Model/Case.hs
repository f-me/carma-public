module Carma.Model.Case
       (Case(..)
       ,caseSearchParams
       ) where

import Data.Text (Text)
import Data.Aeson as Aeson
import qualified Data.Map as Map

import Data.Model as Model
import Data.Model.View as View
import Data.Model.Types ((:@))

import Carma.Model.Case.Type as Case
import Carma.Model.Colors as Color
import Carma.Model.Search as S
import Carma.Model.Types()


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
    ,("subprogram", listOf Case.subprogram)
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
  modelInfo = mkModelInfo Case Case.ident
  modelView = \case
      "search" -> Just
        $ modifyView (searchView caseSearchParams)
        $ [modifyByName "Case_id" (\x -> x { fv_type = "ident" })
          ,modifyByName "files"   (\x -> x { fv_type = "dictionary" })
          ,dict files $ (dictOpt "ExistDict")
                      { dictType    = Just "ComputedDict"
                      , dictBounded = True
                      }
          ]
          ++ caseMod ++ caseDicts
      "full" -> Just
        $ modifyView
          ((defaultView :: ModelView Case) {mv_title = "Кейс"})
          $ caseMod ++ caseDicts ++ caseRo ++ caseOldCRUDHacks
      "new" -> setMainOnly `fmap` (modelView "full" :: Maybe (ModelView Case))
      ""    -> modelView "full" :: Maybe (ModelView Case)
      _ -> Nothing
      where
        setMainOnly mv = mv
          {mv_fields =
             [fv{fv_meta = Map.insert "mainOnly" (Aeson.Bool True) $ fv_meta fv}
             |fv <- mv_fields mv
             ,not $ fv_name fv `elem`
               ["caseAddress_map", "caseAddress_comment", "caseAddress_coords"]
             ]
          }

caseDicts :: [(Text, FieldView -> FieldView) :@ Case]
caseDicts = [
   dict comment $ (dictOpt "Wazzup")
              {dictBounded = False}
  ,dict diagnosis1 $ (dictOpt "Diagnosis1")
  ,dict diagnosis2 $ (dictOpt "Diagnosis2")
              {dictParent = Just $ Model.fieldName diagnosis1}
  ,dict diagnosis3 $ (dictOpt "Diagnosis3")
  ,dict diagnosis4 $ (dictOpt "Diagnosis4")
  ,setType "dictionary" contractIdentifier
  ,dict contractIdentifier $ (dictOpt "") {dictType = Just "ContractsDict"}
  ,dict car_model $ (dictOpt "CarModels")
              {dictParent = Just $ Model.fieldName car_make, dictBounded = True}
  ,dict car_seller $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}
  ,dict car_dealerTO $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}

  ,car_color `completeWith` Color.label
  ]

-- | Mark several new-style dictionaries to use dictionaryStringify,
-- to wrap integers in strings to be compatible with the old CRUD.
caseOldCRUDHacks :: [(Text, FieldView -> FieldView) :@ Case]
caseOldCRUDHacks =
    [ setMeta "dictionaryStringify" (Aeson.Bool True) car_class
    , setMeta "dictionaryStringify" (Aeson.Bool True) car_engine
    , setMeta "dictionaryStringify" (Aeson.Bool True) car_transmission
    , setType "text" car_mileage
    , setType "text" car_makeYear
    , setMeta "dictionaryStringify" (Aeson.Bool True) program
    , setMeta "dictionaryStringify" (Aeson.Bool True) subprogram
    ]

caseRo :: [(Text, FieldView -> FieldView) :@ Case]
caseRo = [
   readonly callDate
  ,readonly callTaker
  ,readonly psaExported
  ]

caseMod :: [(Text, FieldView -> FieldView) :@ Case]
caseMod = [
   transform "capitalize" contact_name
  ,transform "capitalize" contact_ownerName
  ,transform "uppercase"  car_vin
  ,transform "uppercase"  car_plateNum
  ,setMeta "regexp" "plateNum" car_plateNum
  ,setMeta "regexp" "vin" car_vin
  ,setMeta "regexp" "year" car_makeYear

  ,invisible contract
  ,setType "text" contract

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

  ,setMeta "regexp" "email" contact_email
  ,setMeta "regexp" "email" contact_ownerEmail
  ,setMeta "regexp" "date" car_buyDate

  ,mainToo contact_contactOwner
  ,mainToo car_plateNum

  ,setMeta "dictionaryParent"
   (Aeson.String $ Model.fieldName program) subprogram

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
  ++ [ setMeta "cityField" (Aeson.String $ Model.fieldName city) caseAddress_map
     , setMeta "cityField" (Aeson.String $ Model.fieldName city) caseAddress_coords]
