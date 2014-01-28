{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.VIN.SQL

where

import qualified Blaze.ByteString.Builder.Char8 as BZ

import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField

import           Data.Model
import           Carma.Model.Partner
import           Carma.Model.SubProgram

import           Carma.VIN.Base


-- | Internal field name for a CSV column. It is neither external
-- column title, nor Contract field name.
type InternalName = T.Text


-- | Produce unique internal names from a CSV header.
mkInternalNames :: [ColumnTitle] -> [InternalName]
mkInternalNames ns =
    Data.List.take (length ns) $
    map (\(tpl, num) -> fromString (tpl ++ show num)) $
    zip (repeat "field") ([1..] :: [Int])


partnerTable :: T.Text
partnerTable = tableName $ (modelInfo :: ModelInfo Partner)


subProgramTable :: T.Text
subProgramTable = tableName $ (modelInfo :: ModelInfo SubProgram)


-- | First argument of @concat_ws@, quoted.
joinSymbol :: T.Text
joinSymbol = "' '"


makeProtoTable :: [InternalName] -> Query
makeProtoTable names =
    fromString $
    "CREATE TEMPORARY TABLE vinnie_proto (" ++
    (intercalate "," $ map (\t -> (T.unpack t) ++ " text") names) ++
    ");"


copyProtoStart :: Query
copyProtoStart =
    [sql|COPY vinnie_proto FROM STDIN (FORMAT CSV, HEADER 1, DELIMITER ';');|]


protoUpdate :: Query
protoUpdate = [sql|UPDATE vinnie_proto SET ? = ?;|]


sqlCast :: T.Text -> T.Text -> T.Text
sqlCast val t = T.concat [val, "::", t]


sqlCommas :: [T.Text] -> T.Text
sqlCommas ts = T.intercalate "," ts


-- | Replace dictionary label references with dictionary element id.
-- Query parameters: target dictionary table, field name (3 times).
--
-- TODO Eliminate the need for 3 identical query parameters.
protoDictLookup :: Query
protoDictLookup =
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE ? NOT IN
     (SELECT 
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms))))
      FROM "?");
     WITH dict AS
     (SELECT id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms)))) AS label
      FROM "?")
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict WHERE length(lower(trim(both ' ' from ?))) > 0 AND
                     dict.label=lower(trim(both ' ' from ?));
     |]


-- | Replace partner label/code references with partner ids. Query
-- parameters: partner table name, field name (3 times).
protoPartnerLookup :: Query
protoPartnerLookup =
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE ? NOT IN
     (SELECT 
      lower(trim(both ' ' from (unnest(ARRAY[name, code]))))
      FROM "?");
     WITH dict AS
     (SELECT id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[name, code])))) AS label
      FROM "?")
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict WHERE length(lower(trim(both ' ' from ?))) > 0 AND
                     dict.label=lower(trim(both ' ' from ?));
     |]


-- | TODO Move @lower(trim(both ' ' from $1))@ to functions (when the
-- performance issue is resolved).
installFunctions :: Query
installFunctions =
    [sql|
     CREATE OR REPLACE FUNCTION pg_temp.dateordead(text) RETURNS text AS $$
     DECLARE x DATE;
     BEGIN
         x = $1::DATE;
         RETURN $1;
     EXCEPTION WHEN others THEN
         RETURN null;
     END;
     $$ LANGUAGE plpgsql;

     CREATE OR REPLACE FUNCTION pg_temp.numordead(text) RETURNS text AS $$
     DECLARE x NUMERIC;
     BEGIN
         x = $1::NUMERIC;
         RETURN $1;
     EXCEPTION WHEN others THEN
         RETURN null;
     END;
     $$ LANGUAGE plpgsql;
     |]


-- | Create a purgatory for new contracts with schema identical to
-- Contract model table.
makeQueueTable :: Query
makeQueueTable =
    [sql|
     CREATE TEMPORARY TABLE vinnie_queue
     AS (SELECT * FROM "Contract" WHERE 'f');
     ALTER TABLE vinnie_queue ADD COLUMN errors text[];
     |]


setCommitter :: Query
setCommitter = [sql|UPDATE vinnie_queue SET committer = ?;|]


transferQueue :: Query
transferQueue =
    [sql|
     INSERT INTO vinnie_queue (?)
     SELECT ?
     FROM vinnie_proto;
     |]


-- | Set default values for a column in queue table. Parameters: field
-- name, default value, field name again.
setQueueDefaults :: Query
setQueueDefaults =
    [sql|
     UPDATE vinnie_queue SET ? = ?
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Add error to every row where a provided field is empty (used to
-- mark empty required fields). Parameters: error message, field name.
markEmptyRequired :: Query
markEmptyRequired =
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Add error to every row where a provided field is empty (used to
-- mark empty required fields). Parameters: list of all Contract model
-- field names (twice).
deleteDupes :: Query
deleteDupes =
    [sql|
     DELETE FROM vinnie_queue
     WHERE row_to_json(row(?))::text IN (SELECT row_to_json(row(?))::text FROM "Contract");
     |]


-- | Transfer all contracts from queue to the real table. Parameters:
-- list of all Contract model field names (twice).
transferContracts :: Query
transferContracts =
    [sql|
     INSERT INTO "Contract" (?)
     SELECT ?
     FROM vinnie_queue;
     |]


newtype PlainText = PT InternalName

instance ToField PlainText where
    toField (PT i) = Plain $ BZ.fromText i


data RowError = EmptyRequired Text
              deriving Show

instance ToField RowError where
    toField (EmptyRequired t) =
        toField $ T.concat ["Обязательное поле «", t, "» не распознано"]
