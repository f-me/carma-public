
module Carma.Model.SmsTemplate where


import Data.Text (Text)
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.Types()


data SmsTemplate = SmsTemplate
  {ident    :: PK Int SmsTemplate ""
  ,label    :: F Text "label"    "Название шаблона"
  ,isActive :: F Bool "isActive" "Активный?"
  ,text     :: F Text "text"     "Текст шаблона"
  }
  deriving Typeable

instance Model SmsTemplate where
  type TableName SmsTemplate = "SmsTemplate"
  modelInfo = mkModelInfo SmsTemplate ident
  modelView _ = modifyView defaultView [textarea text]


order :: IdentI SmsTemplate
order = Ident 1

cancel :: IdentI SmsTemplate
cancel = Ident 2

complete :: IdentI SmsTemplate
complete = Ident 3

create :: IdentI SmsTemplate
create = Ident 13
