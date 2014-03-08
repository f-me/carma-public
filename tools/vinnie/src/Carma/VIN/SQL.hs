{-|

SQL helpers used during VIN import process.

-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.VIN.SQL

where

import qualified Blaze.ByteString.Builder.Char8 as BZ

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import           Data.Int
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import           Database.PostgreSQL.Simple hiding ( execute
                                                   , execute_
                                                   , query
                                                   , query_)
import qualified Database.PostgreSQL.Simple as PG ( execute
                                                  , execute_
                                                  , query
                                                  , query_)
import qualified Database.PostgreSQL.Simple.Copy as PG (copy)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Data.Model
import           Carma.Model.Contract
import           Carma.Model.Partner
import           Carma.Model.Program
import           Carma.Model.SubProgram
import           Carma.Model.VinFormat

import           Carma.VIN.Base


-- | Internal field name for a CSV column. It is neither external
-- column title, nor Contract field name.
type InternalName = T.Text


-- | Text wrapper with a non-quoting 'ToField' instance.
newtype PlainText = PT InternalName

instance ToField PlainText where
    toField (PT i) = Plain $ BZ.fromText i


-- | Works almost like '(:.)' for 'ToField' instances. Start with `()`
-- and append as many fields as needed:
--
-- > () :* f1 :* f2 :* f3
--
-- Initial `()` saves the type hassle.
--
-- This is copied from carma's Snaplet.Geo
data a :* b = a :* b deriving (Eq, Ord, Show, Read)

infixl 3 :*

instance (ToRow a, ToField b) => ToRow (a :* b) where
    toRow (a :* b) = toRow $ a :. (Only b)


execute :: ToRow p => Query -> p -> Import Int64
execute q p = asks connection >>= \conn -> liftIO $ PG.execute conn q p


execute_ :: Query -> Import Int64
execute_ q = asks connection >>= \conn -> liftIO $ PG.execute_ conn q


query :: (FromRow r, ToRow p) => Query -> p -> Import [r]
query q p = asks connection >>= \conn -> liftIO $ PG.query conn q p


query_ :: (FromRow r) => Query -> Import [r]
query_ q = asks connection >>= \conn -> liftIO $ PG.query_ conn q


copy :: ToRow p => Query -> p -> Import ()
copy q p = asks connection >>= \conn -> liftIO $ PG.copy conn q p


errorsTitle :: Text
errorsTitle = "Ошибки"


-- | Produce unique internal names from a CSV header.
mkInternalNames :: [ColumnTitle] -> [InternalName]
mkInternalNames columns =
    map (\(col, num) ->
             if col == errorsTitle
             then "errors"
             else fromString ("field" ++ show num)) $
    zip columns ([1..] :: [Int])


-- | Name of id field which maps proto entries to queue.
kid :: InternalName
kid = "id"


-- | First argument of @concat_ws@, quoted.
joinSymbol :: T.Text
joinSymbol = "' '"


sqlCast :: T.Text -> T.Text -> T.Text
sqlCast val t = T.concat [val, "::", t]


sqlCommas :: [T.Text] -> T.Text
sqlCommas ts = T.intercalate "," ts


getProgram :: Int
           -- ^ Subprogram id.
           -> Import [Only Int]
getProgram sid =
    query
    [sql|
     SELECT p.id FROM "?" p, "?" s
     WHERE p.id = s.parent AND s.id = ?;
     |] ( PT $ tableName $ (modelInfo :: ModelInfo Program)
        , PT $ tableName $ (modelInfo :: ModelInfo SubProgram)
        , sid)


contractTable :: PlainText
contractTable = PT $ tableName $
                (modelInfo :: ModelInfo Contract)


-- | Loadable Contract field names.
contractFields :: [Text]
contractFields =
    map (\(FFAcc (FA c) _ _ _ _ _) -> fieldName c) vinFormatAccessors


-- | Create temporary pristine and proto tables for CSV data.
createCSVTables :: [InternalName] -> Import ()
createCSVTables names =
    execute_ (mkProto "vinnie_pristine") >>
    execute_ (mkProto "vinnie_proto") >>
    return ()
    where
      mkProto table = fromString $ concat
                      [ "CREATE TEMPORARY TABLE "
                      , table
                      , " ("
                      , intercalate "," $
                        (map (\t -> (T.unpack t) ++ " text") names) ++
                        [(T.unpack kid) ++ " serial"]
                      , ");"
                      ]


-- | Read CSV into pristine table.
copyPristineStart :: [InternalName] -> Import ()
copyPristineStart inames =
  copy
  [sql|
   COPY vinnie_pristine (?)
   FROM STDIN (FORMAT CSV, HEADER 1, DELIMITER ';');
   |] (Only $ PT $ sqlCommas inames)


-- | Initiate COPY FROM for report.
copyReportStart :: [InternalName] -> Import ()
copyReportStart inames =
    copy
    [sql|
     COPY (SELECT ?, array_to_string(q.errors, ';')
           FROM vinnie_pristine p, vinnie_queue q
           WHERE p.id = q.id AND q.errors IS NOT NULL
           ORDER BY p.id)
     TO STDIN (FORMAT CSV, FORCE_QUOTE *, DELIMITER ';');
     |] (Only $ PT $ sqlCommas $ delete "errors" inames)


-- | Copy pristine table data to proto.
pristineToProto :: Import Int64
pristineToProto =
   execute_
   [sql|
    INSERT INTO vinnie_proto
    SELECT *
    FROM vinnie_pristine;
    |]


protoUpdate :: Query
protoUpdate = [sql|UPDATE vinnie_proto SET ? = ?;|]


protoUpdateWithFun :: InternalName
                   -> Text
                   -> [Text]
                   -> Import Int64
protoUpdateWithFun iname fun args =
    execute protoUpdate
                ( PT iname
                , PT $ T.concat [fun, "(", sqlCommas args, ")"])


-- | Clear field values which do not match a regular expression (case
-- insensitive).
protoCheckRegexp :: Text
                 -- ^ Field name.
                 -> Text
                 -- ^ Regular expression.
                 -> Import Int64
protoCheckRegexp fname regexp =
    execute
    [sql|
     UPDATE vinnie_proto SET ? = trim(both ' ' from ?);
     UPDATE vinnie_proto SET ? = null WHERE ? !~* ?;
     |] ( PT fname
        , PT fname
        , PT fname
        , PT fname
        , regexp)


-- | Replace dictionary label references with dictionary element ids.
-- Clear bad references.
protoDictLookup :: InternalName
                -- ^ Field name.
                -> Text
                -- ^ Dictionary table name.
                -> Import Int64
protoDictLookup iname dictTableName =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
     -- TODO label/synonyms field names
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms))))
      FROM "?");

     WITH dict AS
     (SELECT DISTINCT ON (label) id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[label] || synonyms)))) AS label
      FROM "?")
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict WHERE length(lower(trim(both ' ' from ?))) > 0 AND
                     dict.label=lower(trim(both ' ' from ?));
     |] ( PT iname
        , PT iname
        , PT dictTableName
        , PT dictTableName
        , PT iname
        , PT iname
        , PT iname)


-- | Replace partner label/code references with partner ids. Clear bad
-- references.
--
-- TODO Speed up first query.
protoPartnerLookup :: InternalName
                   -> Import Int64
protoPartnerLookup iname =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
      -- TODO name/code/synonyms field names
      lower(trim(both ' ' from (unnest(ARRAY[name, code] || synonyms))))
      FROM "?");

     WITH dict AS
     (SELECT DISTINCT ON (label) id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[name, code] || synonyms))))
       AS label
      FROM "?")
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict WHERE length(lower(trim(both ' ' from ?))) > 0 AND
                     dict.label=lower(trim(both ' ' from ?));
     |] ( PT iname
        , PT iname
        , partnerTable
        , partnerTable
        , PT iname
        , PT iname
        , PT iname)
        where
          -- TODO partner{Name,Code}
          partnerTable = PT $ tableName $ (modelInfo :: ModelInfo Partner)


-- | Replace subprogram label references with subprogram ids. Only
-- children of a provided program are selected. If no reference found,
-- default to a provided subprogram .
protoSubprogramLookup :: Int
                      -- ^ Program id.
                      -> Maybe Int
                      -- ^ Default subprogram id.
                      -> InternalName
                      -> Import Int64
protoSubprogramLookup pid sid iname =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=? WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
      lower(trim(both ' ' from (unnest(ARRAY[s.label] || synonyms))))
      FROM "?" s, "?" p WHERE s.parent = p.id AND p.id = ?);

     -- TODO label/parent fields
     WITH dict AS
     (SELECT DISTINCT s.id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[s.label] || synonyms)))) AS label
      FROM "?" s, "?" p WHERE s.parent = p.id AND p.id = ?)
     UPDATE vinnie_proto SET ? = dict.did
     FROM dict WHERE length(lower(trim(both ' ' from ?))) > 0 AND
                     dict.label=lower(trim(both ' ' from ?));
     |] (()
         :* PT iname
         :* sid
         :* PT iname
         :* subProgramTable
         :* programTable
         :* pid
         :* subProgramTable
         :* programTable
         :* pid
         :* PT iname
         :* PT iname
         :* PT iname)
        where
          programTable = PT $ tableName $ (modelInfo :: ModelInfo Program)
          subProgramTable = PT $ tableName $ (modelInfo :: ModelInfo SubProgram)


-- | TODO Move @lower(trim(both ' ' from $1))@ to functions (when the
-- performance issue is resolved).
installFunctions :: Import Int64
installFunctions =
    execute_
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
createQueueTable :: Import Int64
createQueueTable =
    execute
    [sql|
     CREATE TEMPORARY TABLE vinnie_queue
     AS (SELECT * FROM "?" WHERE 'f');
     ALTER TABLE vinnie_queue ADD COLUMN errors text[];
     |] (Only contractTable)


-- | Set committer and subprogram (if not previously set) for
-- contracts in queue.
setSpecialDefaults :: Int -> Maybe Int -> Import Int64
setSpecialDefaults cid sid =
    execute
    [sql|
     -- TODO committer name
     UPDATE vinnie_queue SET committer = ?;
     UPDATE vinnie_queue SET subprogram = ? WHERE subprogram IS NULL;
     |] (cid, sid)


-- | Transfer from proto table to queue table, casting text values
-- from proto table to actual types used by Contract model.
transferQueue :: [Text]
              -- ^ Internal column names of proto table, with type
              -- casting specifiers (@field17::int@).
              -> [Text]
              -- ^ Loadable Contract columns (@mileage@).
              -> Import Int64
transferQueue inames fnames =
    execute [sql|
     INSERT INTO vinnie_queue (?)
     SELECT ?
     FROM vinnie_proto;
     |] ( PT $ sqlCommas (kid:fnames)
        , PT $ sqlCommas (kid:inames))


-- | Set default values for a column in queue table. Parameters: field
-- name, default value, field name again.
setQueueDefaults :: Query
setQueueDefaults =
    [sql|
     UPDATE vinnie_queue SET ? = ?
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Add error to every row with missing identifier fields.
markMissingIdentifiers :: Import Int64
markMissingIdentifiers =
    execute
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE length(lower(trim(both ' ' from concat(?)))) = 0;
     |] ( NoIdentifiers
        , PT $ sqlCommas identifierNames)


-- | Add error to every row where a provided field is empty (used to
-- mark empty required fields). Parameters: error message, field name.
markEmptyRequired :: Query
markEmptyRequired =
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Calculate how many erroneous rows are in queue table.
countErrors :: Import Int64
countErrors = do
  [Only bad] <-
      query_ [sql|SELECT count(1) FROM vinnie_queue WHERE errors IS NOT NULL;|]
  return bad


-- | Delete all rows from the queue which are already present in
-- Contract table.
deleteDupes :: Import Int64
deleteDupes =
    execute
    [sql|
     DELETE FROM vinnie_queue
     WHERE row_to_json(row(?))::text IN
     (SELECT row_to_json(row(?))::text FROM "?");
     |] ( PT $ sqlCommas contractFields
        , PT $ sqlCommas contractFields
        , contractTable)


-- | Transfer all contracts w/o errors from queue to live Contract
-- table.
transferContracts :: Import Int64
transferContracts =
    execute
    [sql|
     INSERT INTO "?" (?)
     SELECT DISTINCT ?
     FROM vinnie_queue WHERE errors IS NULL;
     |] ( contractTable
        , PT $ sqlCommas $ "committer":contractFields
        , PT $ sqlCommas $ "committer":contractFields)


data RowError = EmptyRequired Text
              | NoIdentifiers
              | NoSubprogram
                deriving Show

instance ToField RowError where
    toField (EmptyRequired t) =
        toField $ T.concat ["Обязательное поле «", t, "» не распознано"]
    toField (NoIdentifiers) =
        toField $ T.concat ["Ни одно из полей-идентификаторов не распознано"]
    toField NoSubprogram =
        toField $ T.concat ["Подпрограмма не распознана"]
