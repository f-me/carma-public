{-# LANGUAGE Rank2Types #-}

module Snaplet.Search (Search, searchInit)  where

import           Control.Lens
import           Data.Pool

import           Data.Aeson

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)
import           Snap.Snaplet.PostgresqlSimple (Postgres(..))
import           Database.PostgreSQL.Simple as PG

import           Snaplet.DbLayer.Types (DbLayer)

import           Utils.HttpErrors

import Snaplet.Search.Case
import Snaplet.Search.Call
import Snaplet.Search.Contract
import Snaplet.Search.Types
import Snaplet.Search.Utils

search :: ToJSON t
       => SearchHandler b (Either String (SearchResult t)) -> SearchHandler b ()
search = (>>= either (finishWithError 500) writeJSON)

searchInit
  :: Pool Connection
  -> Snaplet (AuthManager b)
  -> Lens' b (Snaplet (DbLayer b))
  -> SnapletInit b (Search b)
searchInit conn sessionMgr dbl = makeSnaplet "search" "Search snaplet" Nothing $ do
  pg <- nestSnaplet "db" postgres $ makeSnaplet "postgresql-simple" "" Nothing $
        return $ Postgres conn
  addRoutes [ ("case",     method POST $ search caseSearch)
            , ("call",     method POST $ search callSearch)
            , ("contract", method POST $ search contractSearch)
            , (":q/contract.csv", method GET $ portalHandler contractCSV)
            , ("portal", method POST $ search portalSearch)
            ]
  return $ Search conn pg sessionMgr dbl
