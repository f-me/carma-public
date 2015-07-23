{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}


module Carma.Model.Usermeta where

import           Control.Applicative
import           Data.Int (Int64)
import           Data.Text (Text, intercalate, unpack, append)
import           Data.Typeable
import           Data.Vector     (Vector)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Calendar (Day)
import           Data.String (fromString)
import qualified Data.Aeson as Aeson
import           Text.Printf

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types

import           Data.Model
import           Data.Model.Patch (Patch)
import qualified Data.Model.Patch as P
import           Data.Model.TH
import           Data.Model.View
import           Data.Model.CRUD

import           Carma.Model.LegacyTypes (Phone, Password)
import           Carma.Model.Types (UserStateVal)
import           Carma.Model.City (City)
import           Carma.Model.Role (Role)
import           Carma.Model.BusinessRole (BusinessRole)

import {-#SOURCE#-} Carma.Model.SubProgram.Type (SubProgram)

data Usermeta = Usermeta
  { ident        :: PK Int Usermeta      "Данные о пользователе"
  , uid          :: F Int                "uid"     "Snap-идентификатор"
  , isActive     :: F Bool               "isActive" "Активен"
  , realName     :: F Text               "realName" "ФИО пользователя"
  , login        :: F Text               "login"    "Логин"
  , password     :: EF Password          "password" "Пароль"
  , roles        :: F (Vector (IdentI Role)) "roles" "Роли в системе"
  , businessRole :: F (Maybe (IdentI BusinessRole))  "businessRole" "Бизнес-роль"
  , isJack       :: F Bool               "isJack"   "Универсал"
  , bocities     :: F (Vector (IdentI City)) "bocities" "Города"
  , boprograms   :: F (Vector Text)      "boprograms" "Программы"
  , subPrograms  :: F (Vector (IdentI SubProgram)) "subPrograms" "Подпрограммы"
  , isDealer     :: F Bool               "isDealer" "Дилер"
  , workPhone    :: F Phone              "workPhone" "Рабочий телефон"
  , workPhoneSuffix
                 :: F Text               "workPhoneSuffix" "Добавочный номер"
  , mobilePhone  :: F Phone              "mobilePhone"     "Мобильный телефон"
  , homePhone    :: F Phone              "homePhone"       "Домашний телефон"
  , email        :: F Text               "email"           "E-mail"
  , birthday     :: F (Maybe Day)        "birthday"        "День рождения"
  , group        :: F Text               "grp"             "Группа"
  , position     :: F Text               "position"        "Должность"

  , delayedState :: F (Maybe UserStateVal) "delayedState" "Отложенный статус"
  , currentState      :: EF UserStateVal "currentState"      "Текущий статус"
  , currentStateCTime :: EF UTCTime      "currentStateCTime" ""
  -- Some stuff for internal use on client, can be used instead of localStorage
  , stuff        :: F Aeson.Value "stuff" ""
  , showKPI      :: F Bool "showKPI" "Отслеживать KPI"
  , lastAvayaSnapshot
    :: F Aeson.Value "lastAvayaSnapshot"
       "Последнее известное состояние AVAYA-агента"
  } deriving Typeable


mkIdents [t|Usermeta|]
 [ ("psa", 387)
 , ("arc", 728)
 , ("admin", 90)
 ]


instance Model Usermeta where
  type TableName Usermeta = "usermetatbl"
  modelInfo = mkModelInfo Usermeta ident
    `customizeRead`             fillCurrentState
    `replaceReadManyWithFilter` fillStatesForAll
  modelView = \case
    "" -> Just $ modifyView (defaultView)
      [ invisible uid
      , invisible currentStateCTime
      , invisible stuff
      , invisible lastAvayaSnapshot
      , required realName
      , required login
      , required password
      , dict roles $ (dictOpt "visibleRoles")
          {dictType = Just "ComputedDict"
          ,dictBounded = True
          }
      , dict subPrograms $ (dictOpt "prefixedSubPrograms")
          {dictType = Just "ComputedDict"
          ,dictBounded = True
          }
      , dict boprograms $ (dictOpt "Program")
          {dictType = Just "ModelDict"
          ,dictBounded = True
          }
      , widget "onlyServiceBreak" delayedState
      , infoText "isDealer" isDealer
      , infoText "isJack" isJack
      , infoText "userSubprograms" subPrograms
      , infoText "extension" workPhoneSuffix
      , infoText "boPrograms" boprograms
      , infoText "boCities" bocities
      ]
    _  -> Nothing


fillCurrentState :: Patch Usermeta -> IdentI Usermeta -> PG.Connection
                 -> IO (Patch Usermeta)
fillCurrentState p idt c = do
  st <- PG.query c [sql|
          SELECT ctime, state
          FROM "UserState" WHERE userId = ?
          ORDER BY id DESC LIMIT 1
        |] (PG.Only idt)
  case st of
    [(ctime, state)] -> return $
                        P.put currentState      state $
                        P.put currentStateCTime ctime $
                        p
    _                -> return p


fillStatesForAll :: Int64 -> Int64 -> [(Text, Text)] -> PG.Connection
                 -> IO [Patch Usermeta]
fillStatesForAll lim off _ c
  = map addStates <$> PG.query c (fromString q) (lim, off)
  where
    -- FIXME: make normal query generator already
    q = printf allUsrsQ $ unpack $ intercalate ", " $
        map (append "u.") fieldNames
    fieldNames = map fd_name $ onlyDefaultFields $ modelFields mInfo
    mInfo = modelInfo :: ModelInfo Usermeta

    addStates
      :: Patch Usermeta :. (Maybe UserStateVal, Maybe UTCTime)
      -> Patch Usermeta
    addStates (um :. (Just s, Just ct)) =
        P.put currentState s $ P.put currentStateCTime ct um
    addStates (um :. _) = um


-- | Select all users with their current states
allUsrsQ :: String
allUsrsQ =
 "SELECT %s, s.state, s.ctime "                                     ++
 "FROM usermetatbl u "                                              ++
 "LEFT JOIN (SELECT DISTINCT ON (userid) id, state, ctime, userid " ++
            "FROM \"UserState\" ORDER BY userid, id DESC) s "       ++
 "ON u.id = s.userid "                                              ++
 "LIMIT ? OFFSET ? ;"
