module Carma.Model.UserState (UserState(..), UserStateVal(..)) where

import Data.Typeable
import Data.Time.Clock (UTCTime)

import Data.Model
import Data.Model.View

import Carma.Model.Usermeta (Usermeta)
import Carma.Model.Event    (Event)
import Carma.Model.Types

data UserState = UserState
  { ident   :: PK Int UserState    ""
  , ctime   :: F UTCTime           "ctime"   "Время создания"
  , state   :: F UserStateVal      "state"   "Состояние"
  , eventId :: F (IdentI Event)    "eventId" "Событие"
  , userId  :: F (IdentI Usermeta) "userId"  "Пользователь"
  } deriving Typeable

instance Model UserState where
  type TableName UserState = "UserState"
  modelInfo = mkModelInfo UserState ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
