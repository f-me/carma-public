{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|

Proxy model for a subset of legacy usermeta model.

-}

module Carma.Model.Usermeta where

import           Data.Text
import           Data.Typeable
import           Data.Vector
import           Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson

import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           Data.Model
import           Data.Model.Patch (Patch)
import qualified Data.Model.Patch as P
import           Data.Model.TH
import           Data.Model.View
import           Data.Model.CRUD

import           Carma.Model.Types (UserStateVal)
import           Carma.Model.Role hiding (ident)
import           Carma.Model.BusinessRole hiding (ident)

data Usermeta = Usermeta
  { ident    :: PK Int Usermeta          "Данные о пользователе"
  , snapId   :: F Int
                "uid"
                "Snap-идентификатор"
  , label    :: F Text
                "login"
                "Логин"
  , value    :: F (Maybe Text)
                "realName"
                "ФИО пользователя"
  , isDealer :: F Bool
                "isDealer"
                "Дилер"
  -- TODO String-wrapped list of Role ids (to be used until usermeta
  -- is fully migrated to new models)
  , roles    :: F (Maybe (Vector (IdentT Role)))
                "roles"
                "Роли в системе"
  , businessRole :: F (Maybe (IdentT BusinessRole))
                    "businessRole" "Бизнес-роль"
  , delayedState :: F (Maybe UserStateVal)
                 "delayedState"
                 "Отложенный статус"
  , currentState      :: EF UserStateVal "currentState"      "Текущий статус"
  , currentStateCTime :: EF UTCTime      "currentStateCTime" ""
  , programs     :: F (Maybe (Vector Text))
                    "programs" "Подпрограммы"
  , bocities     :: F (Maybe (Vector Text))
                    "bocities" "Города"
  , boprograms   :: F (Maybe (Vector Text))
                    "boprograms" "Программы"
  } deriving Typeable


mkIdents [t|Usermeta|]
 [ ("psa", 387) ]


instance Model Usermeta where
  type TableName Usermeta = "usermetatbl"
  modelInfo = mkModelInfo Usermeta ident
    `customizeRead` fillCurrentState
  modelView = \case
    "" -> Just $ modifyView (defaultView)
          [ setMeta "dictionaryStringify" (Aeson.Bool True)          roles
          , setMeta "dictionaryType"      (Aeson.String "ModelDict") roles
          , setMeta "bounded"             (Aeson.Bool True)          roles

          ,setMeta "dictionaryStringify" (Aeson.Bool True)          businessRole
          ,setMeta "dictionaryType"      (Aeson.String "ModelDict") businessRole
          ,setMeta "bounded"             (Aeson.Bool True)          businessRole

          , setMeta "dictionaryStringify" (Aeson.Bool True)          programs

          , setMeta "dictionaryStringify" (Aeson.Bool True)          bocities

          , setMeta "dictionaryStringify" (Aeson.Bool True)          boprograms

          , dict programs $ (dictOpt "prefixedSubPrograms")
              {dictType = Just "ComputedDict"
              ,dictBounded = True
              }
          , dict bocities $ (dictOpt "DealerCities")
              {dictBounded = True}
          , dict boprograms $ (dictOpt "Program")
              {dictType = Just "ModelDict"
              ,dictBounded = True
              }
          ,widget "onlyServiceBreak" delayedState
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
