{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}

{-|

Proxy model for a subset of legacy usermeta model.

-}

module Carma.Model.Usermeta where

import           Data.Int (Int64)
import           Data.Text (Text, intercalate, unpack, append)
import           Data.Typeable
import           Data.Vector     (Vector)
import           Data.Time.Clock (UTCTime)
import qualified Data.Aeson as Aeson
import           Data.String (fromString)
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
  , isActive :: F (Maybe Bool)
                "isActive"
                "Активен"
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
    `customizeRead`             fillCurrentState
    `replaceReadManyWithFilter` fillStatesForAll
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

fillStatesForAll :: Int64 -> Int64 -> [(Text, Text)] -> PG.Connection
                 -> IO [Patch Usermeta]
fillStatesForAll lim off _ c = do
  usrs :: [(Patch Usermeta) :. (Maybe UserStateVal, Maybe UTCTime)] <-
          PG.query c (fromString q) (lim, off)
  return $ map addStates usrs
      where
        -- FIXME: make normal query generator allready
        q = printf allUsrsQ $ unpack $ intercalate ", " $
            map (append "u.") fieldNames
        fieldNames = map fd_name $ onlyDefaultFields $ modelFields mInfo
        mInfo = modelInfo :: ModelInfo Usermeta
        addStates (um :. (Just s, Just ct)) =
            P.put currentState s $ P.put currentStateCTime ct $ um
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

