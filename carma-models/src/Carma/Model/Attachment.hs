module Carma.Model.Attachment where

import Data.Text
import Data.Time.Clock
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.PgTypes()

data Attachment = Attachment
  { ident
    :: PK Int Attachment "Прикреплённый файл"
  , filename
    :: F Text "filename" "Файл"
  , hash
    :: F Text "hash" "MD5-хеш файла"
  , ctime
    :: F UTCTime "ctime" "Время создания"
  } deriving Typeable

instance Model Attachment where
  type TableName Attachment = "attachmenttbl"
  modelInfo = mkModelInfo Attachment ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
