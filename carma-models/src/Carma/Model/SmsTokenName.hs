module Carma.Model.SmsTokenName where

import Data.Text
import Data.Typeable
import Data.Model
import Data.Model.View
import Carma.Model.Types()
import Carma.Model.PgTypes()

data SmsTokenName = SmsTokenName
  { ident   :: PK Int SmsTokenName "Переменные шаблонов СМС"
  , label   :: F Text "label" "Описание"
  , varName :: F Text "var_name" "Название переменной"
  }
  deriving Typeable

instance Model SmsTokenName where
  type TableName SmsTokenName = "SmsTokenName"
  modelInfo = mkModelInfo SmsTokenName ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
