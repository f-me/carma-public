module Carma.Model.Case
       (Case(..)
       ,caseSearchParams
       ) where

import Data.Text (Text)
import Data.Aeson as Aeson

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
    ,("city",       listOf Case.city)
    ,("carMake",    listOf Case.car_make)
    ,("callDate",   interval Case.callDate)
    ,("contact",    fuzzy $ matchAny
                    [one Case.contact_name, one Case.contact_ownerName])
    ,("customerComment",
      fuzzy $ one Case.customerComment)
    ,("comment",    one Case.comment)
    ,("address",    fuzzy $ one Case.caseAddress_address)
    ,("callTaker",  listOf Case.callTaker)
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
      "" -> Just
        $ modifyView
          ((defaultView :: ModelView Case) {mv_title = "Кейс"})
          $ caseMod ++ caseDicts ++ caseRo ++
          [ widget "inline-uploader" files
          , setMeta "reference-widget" "files" files
          ]
      _ -> Nothing

caseDicts :: [(Text, FieldView -> FieldView) :@ Case]
caseDicts = [
   setMeta "dictionaryParent"
   (Aeson.String $ Model.fieldName diagnosis1) diagnosis2
  ,setType "dictionary" contractIdentifier
  ,dict contractIdentifier $ (dictOpt "") {dictType = Just "ContractsDict"}
  , setMeta "dictionaryParent"
      (Aeson.String $ Model.fieldName car_make)
      car_model
  ,dict car_seller $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}
  ,dict car_dealerTO $ (dictOpt "")
              {dictType = Just "DealersDict", dictBounded = True}

  ,car_color `completeWith` Color.label
  ,setMeta "dictionaryLabel" (Aeson.String "realName") callTaker
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
  ,regexp "plateNum" car_plateNum
  ,regexp "vin" car_vin
  ,regexp "year" car_makeYear

  ,hiddenIdent contract

  ,widget "force-find-dictionary" contractIdentifier

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
  ,readonly caseStatus

  ,regexp "email" contact_email
  ,regexp "email" contact_ownerEmail
  ,regexp "date" car_buyDate

  ,mainToo contact_contactOwner
  ,mainToo car_plateNum

  ,setMeta "dictionaryParent"
   (Aeson.String $ Model.fieldName program) subprogram

   -- FIXME Workaround for #2145
--  ,widget "radio" car_transmission
--  ,widget "radio" car_engine

  ,textarea dealerCause
  ,textarea claim

  ,infoText "carVin" car_vin
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
