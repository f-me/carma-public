module Carma.Model.Call where

import Data.Aeson as Aeson
import Data.Text
import Data.Time
import Data.Typeable
import Data.Model as Model
import Data.Model.Types ((:@))
import Data.Model.View

import Carma.Model.Types           (Coords)
import Carma.Model.Program         (Program)
import Carma.Model.Case            (Case)
import Carma.Model.Usermeta        (Usermeta)
import Carma.Model.CallType        (CallType)
import Carma.Model.CallerType      (CallerType)
import Carma.Model.CallReason      (CallReason)
import Carma.Model.AbuseTarget     (AbuseTarget)
import Carma.Model.Partner         (Partner)

import Carma.Model.LegacyTypes

import Carma.Model.Search as S

callSearchParams :: [(Text, [Predicate Call])]
callSearchParams
  = [ ("phone", fuzzy $ one callerPhone)
    , ("callDate", interval callDate)
    , ("callTaker", one callTaker)
    , ("program", one program)
    -- , ("wazzup", one wazzup)
    , ("callType", one callType)
    , ("caller", fuzzy $ one callerName)
    ]

instance Model Call where
  type TableName Call = "calltbl"
  modelInfo = mkModelInfo Call ident
  modelView = \case
    "search" -> Just $ modifyView (searchView callSearchParams) dicts
    _        -> Just $ modifyView defaultView (metas ++ dicts)

dicts :: [(Text, FieldView -> FieldView) :@ Call]
dicts =
  [ setMeta "dictionaryParent"
    (Aeson.String $ Model.fieldName callerType) callReason
  , setMeta "dictionaryLabel" (Aeson.String "realName") callTaker
  ]

metas :: [(Text, FieldView -> FieldView) :@ Call]
metas =
    [ readonly callDate
    , readonly endDate
    , readonly callType

    , readonly callTaker
    , required callTaker

    , invisible coords
    , invisible address
    , invisible endDate
    , invisible callDate

    , hiddenIdent caseId

    , transform "capitalize" callerName
    , textarea customerComment

    , dict partner $ (dictOpt "allPartners")
          { dictType    = Just "ComputedDict"
          , dictBounded = True
          }

    , widget "callType-special" callType
    ]

data Call = Call
  { ident :: PK Int Call "Номер звонка"
  , callType
    :: F (Maybe (IdentI CallType)) "callType" "Тип звонка"
  , callDate
    :: F UTCTime "callDate" "Дата звонка"
  , endDate
    :: F (Maybe UTCTime) "endDate"  "Дата окончания звонка"
  , callTaker
    :: F (IdentI Usermeta) "callTaker" "Сотрудник РАМК"
  , program
    :: F (Maybe (IdentI Program)) "program" "Программа"
  , callerName
    :: F (Maybe Text) "callerName" "Звонящий"
  , callerPhone
    :: F (Maybe Phone) "callerPhone" "Контактные телефоны"
  , callerType
    :: F (Maybe (IdentI CallerType)) "callerType" "Кто звонит?"
  , callReason
    :: F (Maybe (IdentI CallReason)) "callReason" "Причина обращения"
  , abuseTarget
    :: F (Maybe (IdentI AbuseTarget)) "abuseTarget" "На кого поступила жалоба"
  , coords
    :: F (Maybe Coords) "coords" "Координаты места поломки"
  , address
    :: F (Maybe Text) "address" "Адрес места поломки"
  , partner
    :: F (Maybe (IdentI Partner)) "partner" "Дилер"
  , customerComment
    :: F (Maybe Text) "customerComment" "Комментарий"
  , caseId
    :: F (Maybe (IdentI Case)) "caseId" "Связанный кейс"
  } deriving Typeable
