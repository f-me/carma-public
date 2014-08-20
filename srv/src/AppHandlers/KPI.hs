{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppHandlers.KPI (getStat) where

-- import Control.Applicative
import Data.ByteString (ByteString)

import Snap

import Application
import           Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import Data.Model.Patch
import Carma.Model.KPI
import AppHandlers.Util

getStat :: AppHandler ()
getStat = do
  Just f <- getParam "from"
  Just t <- getParam "to"
  writeJSON =<< selectStat f t

selectStat :: ByteString -> ByteString -> AppHandler [W (Patch StatKPI)]
selectStat from to = do
  query [sql|
  SELECT * FROM
    get_KPI_timeinstate(ARRAY(select id from usermetatbl where isActive = true),
                     tstzrange(?, ?));
  |] (from, to)
