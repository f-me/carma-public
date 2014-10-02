module Carma.Model.Service.Information where

import Data.Text
import Data.Typeable

import Data.Model
import Data.Model.View
import Carma.Model.LegacyTypes
import Carma.Model.Service (Service)


data Information = Information
  { ident      :: PK Int Information ""
  , contact1   :: F (Maybe Text)
                  "contact1"   "Контакт 1"
  , phone1     :: F Phone
                  "phone1"     "Телефон 1"
  , whatToSay1 :: F (Maybe Text)
                  "whatToSay1" "Что сказать 1"
  , contact2   :: F (Maybe Text)
                  "contact2"   "Контакт 2"
  , phone2     :: F Phone
                  "phone2"     "Телефон 2"
  , whatToSay2 :: F (Maybe Text)
                  "whatToSay2" "Что сказать 2"
  , contact3   :: F (Maybe Text)
                  "contact3"   "Контакт 3"
  , phone3     :: F Phone
                  "phone3"     "Телефон 3"
  , whatToSay3 :: F (Maybe Text)
                  "whatToSay3" "Что сказать 3"
  }
  deriving Typeable

instance Model Information where
  type TableName Information = "informationtbl"
  type Parent Information = Service
  parentInfo = ExParent modelInfo
  modelInfo = mkModelInfo Information ident
  modelView v = case parentView v :: Maybe (ModelView Information) of
    Nothing -> Nothing
    Just mv -> Just $ mv {mv_title = "Информирование о происшествии"}
