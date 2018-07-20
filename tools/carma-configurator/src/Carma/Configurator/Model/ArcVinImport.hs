{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Carma.Configurator.Model.ArcVinImport
     ( ArcVinImport (..)
     , ArcVinImportArc (..)
     , ArcVinImportPostgreSQL (..)
     , ArcVinImportReportEmail (..)
     ) where

import           GHC.Generics (Generic)

import           Data.Word (Word64, Word16)
import           Data.Text (Text)
import           Data.Aeson

import           Carma.Configurator.Types


data ArcVinImport
   = ArcVinImport
   { format       :: Word64
   , committer    :: Word64
   , arc          :: ArcVinImportArc
   , postgresql   :: ArcVinImportPostgreSQL
   , report_email :: ArcVinImportReportEmail
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data ArcVinImportArc
   = ArcVinImportArc
   { user :: Text
   , host :: Text
   , dir  :: Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data ArcVinImportPostgreSQL
   = ArcVinImportPostgreSQL
   { host     :: Text
   , port     :: Word16
   , user     :: Text
   , password :: Text
   , db_name  :: Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)


data ArcVinImportReportEmail
   = ArcVinImportReportEmail
   { from    :: Email
   , to      :: [Email]
   , subject :: Text
   } deriving (Show, Eq, Generic, FromJSON, ToJSON)
