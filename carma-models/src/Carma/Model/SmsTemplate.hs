
module Carma.Model.SmsTemplate where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View


data SmsTemplate = SmsTemplate
  { label    :: F Text "label"    "Название шаблона"
  , isActive :: F Bool "isActive" "Активный?"
  , text     :: F Text "text"     "Текст шаблона"
  }
  deriving Typeable

instance Model SmsTemplate where
  type TableName SmsTemplate = "SmsTemplate"
  modelInfo = mkModelInfo SmsTemplate
  modelView _ = modifyView defaultView [textarea text]


order :: Ident SmsTemplate
order = Ident 1

cancel :: Ident SmsTemplate
cancel = Ident 2

complete :: Ident SmsTemplate
complete = Ident 3

create :: Ident SmsTemplate
create = Ident 13
