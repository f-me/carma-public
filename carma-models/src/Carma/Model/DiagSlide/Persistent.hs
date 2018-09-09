{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Carma.Model.DiagSlide.Persistent where

import           Data.Typeable
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.Aeson (Value)

import           Database.Persist.TH

import           Database.Persist.Postgresql.JSON ()
                 -- ^ @PersistFieldSql Value@ instance

import           Carma.Model.LegacyTypes (Reference)


mkPersist sqlSettings [persistLowerCase|
DiagSlide sql=DiagSlide
  ctime      UTCTime          sql=ctime
  header     Text             sql=header
  body       Text             sql=body
  resources  Value            sql=resources
  answers    Value            sql=answers
  actions    Value            sql=actions
  isRoot     Bool             sql=isroot
  isActive   Bool             sql=isactive
  files      Reference Maybe  sql=files

  deriving Typeable Show
|]
