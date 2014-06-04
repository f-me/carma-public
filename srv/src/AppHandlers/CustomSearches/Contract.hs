{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           Carma.Model.CarMake
import           Carma.Model.CarModel
import           Carma.Model.Case as Case
import           Carma.Model.Contract as C
import           Carma.Model.Program as P
import           Carma.Model.SubProgram as S hiding (field)
import           Carma.Model.Types (TInt)

import           Application
import           AppHandlers.Util
import           AppHandlers.CustomSearches.Contract.Base
import           Util hiding (withPG)


$(deriveJSON defaultOptions ''(,,,,,,))


extraContractFieldNames :: [Text]
extraContractFieldNames = map fieldNameE extraContractFields


-- | Wrapper for 'searchContracts' query results, a subset of
-- 'Contract' fields with @expired@ flag.
--
-- 'FromRow'/'ToJSON' instance uses field instances from carma-models.
data SearchResult =
    SearchResult { cid         :: (IdentI Contract)
                 , expired     :: Maybe Bool
                 -- ^ Name of a field which matched.
                 , identifiers :: $(fieldTypesQ C.identifiers)
                 , extras      :: $(fieldTypesQ extraContractFields)
                 }

instance FromRow SearchResult where
    fromRow = SearchResult <$> field <*> field <*> fromRow <*> fromRow

instance ToJSON SearchResult where
    toJSON (SearchResult i e vals (cm, cl, ml, sp, d)) =
        object $ [ "id"       .= i
                 , "_expired" .= e
                 ] ++ (zip C.identifierNames listVals)
                   ++ (zip extraContractFieldNames [ toJSON cm
                                                   , toJSON cl
                                                   , toJSON ml
                                                   , toJSON sp
                                                   , toJSON d])
        where
          jsonVals = toJSON vals
          -- Assume that if identifierTypes is a tuple, its ToJSON
          -- instance produces a list of Values.
          listVals =
              case jsonVals of
                A.Array vec -> V.toList vec
                _           -> error "(toJSON identifierTypes) is broken"


-- | Read @query@, @case@, @program@ (optional), @subprogram@
-- (optional), @limit@ (defaults to 100) parameters and return list of
-- contracts with matching identifier fields. Every result in the list
-- contains a subset of contract fields and @_expired@ which is a
-- boolean flag indicating whether a contract is expired or not,
-- compared to callDate field of the provided case.
--
-- TODO Try rewriting the query with carma-models SQL. @_expired@ is
-- the only reason we build a custom query here and use SearchResult
-- wrapper type.
searchContracts :: AppHandler ()
searchContracts = do
  pid <- getIntParam "program"
  sid <- getIntParam "subprogram"
  limit <- fromMaybe 100 <$> getIntParam "limit"
  q <- fromMaybe (error "No search query provided") <$> getParam "query"
  caseId <- fromMaybe (error "No case number provided") <$> getIntParam "case"

  ml <- gets $ searchMinLength . options
  when (B.length q < ml) $ error "Search query is too short"

  -- Form query template and all of its parameters. Contract
  -- identifiers (length M) and extraContractFields (length N) define
  -- what fields are included in the result.
  let -- Predicate which filters contracts by one field. Parameters
      -- (2): field name, query string.
      fieldPredicate =
          "(? ILIKE '%' || ? || '%')"
      fieldParams = zip (map PT C.identifierNames) $ repeat q
      totalQuery = intercalate " "
          [ "SELECT DISTINCT ON(c.id) c.id,"
          -- 4 parameters: case callDate name, contract start/end date
          -- field name, case callDate name
          , "((case.? < ?) or (? < case.?)),"
          -- M + N more parameters: selected fields.
          , intercalate "," $
            map (const "c.?") selectedFieldsParam
          -- 2 parameters: Contract table name, Case table name
          , "FROM \"?\" c, \"?\" case"
          -- 3 more parameters: SubProgram table name, Contract
          -- subprogram field, subprogram id field.
          , "JOIN \"?\" s ON c.? = s.?"
          -- 3 more parameters: Program table name, SubProgram parent
          -- field, program id field.
          , "JOIN \"?\" p ON s.? = p.?"
          , "WHERE"
          , "("
          -- 2*M parameters: identifier fields and query
          , intercalate " OR " $
            map (const fieldPredicate) C.identifierNames
          , ")"
          -- 3 parameters: flag pair for subprogram id, contract
          -- subprogram field name.
          , "AND (? OR ? = c.?)"
          -- 3 parameters: flag pair for program id, program id field
          -- name.
          , "AND (? OR ? = p.?)"
          -- 2 parameters: dixi and isActive field names
          , "AND c.? and c.?"
          -- 2 parameters: case id field name and case number
          , "AND (case.? = ?)"
          -- 1 parameter: LIMIT value
          , "ORDER BY c.id DESC LIMIT ?;"
          ]
      -- Fields selected from matching rows
      selectedFieldsParam =
          map PT $ C.identifierNames ++ extraContractFieldNames
      contractTable = PT $ tableName $
                      (modelInfo :: ModelInfo C.Contract)
      programTable = PT $ tableName $
                     (modelInfo :: ModelInfo P.Program)
      subProgramTable = PT $ tableName $
                        (modelInfo :: ModelInfo S.SubProgram)

  res <- withPG pg_search $ \c -> query c (fromString totalQuery)
         (()
          -- 4
          :. ( PT $ fieldName Case.callDate
             , PT $ fieldName C.validSince
             , PT $ fieldName C.validUntil
             , PT $ fieldName Case.callDate)
          -- M + N
          :. (selectedFieldsParam)
          -- 2
          :. (contractTable, PT $ tableName (modelInfo :: ModelInfo Case.Case))
          -- 3
          :. Only subProgramTable
          :. (PT $ fieldName C.subprogram, PT $ fieldName S.ident)
          -- 3
          :. Only programTable
          :. (PT $ fieldName S.parent, PT $ fieldName P.ident)
          -- 2*M
          :. (ToRowList fieldParams)
          -- 3
          :. (sqlFlagPair (0::Int) id sid)
          :. (Only $ PT $ fieldName C.subprogram)
          -- 3
          :. (sqlFlagPair (0::Int) id pid)
          :. (Only $ PT $ fieldName P.ident)
          -- 2
          :. (PT $ fieldName C.dixi, PT $ fieldName C.isActive)
          -- 2
          :. (PT $ fieldName Case.ident, caseId)
          -- 1
          :. Only limit)

  writeJSON (res :: [SearchResult])
