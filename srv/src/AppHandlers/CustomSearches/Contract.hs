{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{-|

Contract search.

-}

module AppHandlers.CustomSearches.Contract
    ( searchContracts
    )

where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson as A
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as B
import           Data.List
import           Data.Maybe
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Vector as V

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

import           Snap

import           Data.Model
import           Carma.Model.Contract as C
import           Carma.Model.CarMake
import           Carma.Model.CarModel

import           Application
import           AppHandlers.Util
import           AppHandlers.CustomSearches.Contract.Base
import           Util


$(deriveJSON defaultOptions ''(,,,,,,))


extraContractFieldNames :: [Text]
extraContractFieldNames = map fieldNameE extraContractFields


-- | Wrapper for 'searchContracts' query results, a subset of
-- 'Contract' fields with match type annotation.
--
-- 'FromRow'/'ToJSON' instance uses field instances from carma-models.
data SearchResult =
    SearchResult { cid         :: (IdentI Contract)
                 , match       :: Text
                 , expired     :: Maybe Bool
                 -- ^ Name of a field which matched.
                 , identifiers :: $(fieldTypesQ C.identifiers)
                 , extras      :: $(fieldTypesQ extraContractFields)
                 }

instance FromRow SearchResult where
    fromRow = SearchResult <$> field <*> field <*> field <*> fromRow <*> fromRow

instance ToJSON SearchResult where
    toJSON (SearchResult i t e vals (cm, cl)) =
        object $ [ "id"       .= i
                 , "_match"   .= t
                 , "_expired" .= e
                 ] ++ (zip C.identifierNames listVals)
                   ++ (zip extraContractFieldNames [toJSON cm, toJSON cl])
        where
          jsonVals = toJSON vals
          -- Assume that if identifierTypes is a tuple, its ToJSON
          -- instance produces a list of Values.
          listVals =
              case jsonVals of
                A.Array vec -> V.toList vec
                _           -> error "(ToJSON identifierTypes) is broken"


-- | Read @query@, @program@ (optional), @subprogram@ (optional),
-- @limit@ (defaults to 100) parameters and return list of contracts
-- with matching identifier fields. Every result in the list contains
-- a subset of contract fields, @_match@ field with matched field name
-- and @_expired@ which is a boolean flag indicating whether a
-- contract is expired or not.
--
-- TODO Program/subprogram filtering.
--
-- TODO Try rewriting subqueries with carma-models SQL (possibly
-- joining dictionaries on the server).
searchContracts :: AppHandler ()
searchContracts = do
  pid <- getIntParam "program"
  sid <- getIntParam "subprogram"
  limit' <- getIntParam "limit"
  let limit = fromMaybe 100 limit'
  -- TODO Support non-ByteString search queries?
  q <- fromMaybe (error "No search query provided") <$> getParam "query"

  ml <- gets $ searchMinLength . options
  when (B.length q < ml) $ error "Search query is too short"

  -- Form query template and all of its parameters. Contract
  -- identifiers (length M) and extraContractFields (length N) define
  -- how many subqueries are produced and what fields are included in
  -- the result.

  let -- Search contract by one field. Parameters (5): quoted field
      -- name, Contract table name, field name, query string, limit.
      fieldSubQuery = concat
          [ "(SELECT id, ? as _match FROM \"?\" "
          , "WHERE lower(?) LIKE '%' || lower(?) || '%' LIMIT ?)"
          ]
      -- List of sets of subquery parameters, each having ToRow
      -- instance
      subParams = map (\fn -> (()
                               :* fn
                               :* contractTable
                               :* (PT fn)
                               :* q
                               :* limit))
                  C.identifierNames
      unite = intercalate " UNION "
      totalQuery = intercalate " "
          [ "WITH sources AS ("
          -- 5*M parameters
          , unite (map (const fieldSubQuery) C.identifierNames)
          , ")"
          , "SELECT DISTINCT ON(c.id) c.id, _match,"
          -- 2 more parameters: contract start/end date
          , "((now() < ?) or (? < now())),"
          -- M + N more parameters: field list.
          , intercalate "," $
            map (const "c.?") selectedFieldsParam
          -- 2 more parameters: Contract table name, limit.
          , "FROM sources, \"?\" c"
          , "WHERE c.id IN (SELECT id FROM sources)"
          , "ORDER BY c.id DESC LIMIT ?;"
          ]
      -- Fields selected from matching rows
      selectedFieldsParam =
          map PT $ C.identifierNames ++ extraContractFieldNames
      contractTable = PT $ tableName $
                      (modelInfo :: ModelInfo C.Contract)

  res <- withPG pg_search $ \c -> query c (fromString totalQuery)
         (()
          -- 5*M
          :. (ToRowList subParams)
          :. (fieldName C.validSince, fieldName C.validUntil)
          -- M + N
          :. (selectedFieldsParam)
          -- 2
          :. (Only contractTable
              :* limit))

  writeJSON (res :: [SearchResult])
