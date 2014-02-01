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

import           Database.PostgreSQL.Simple hiding (execute, execute_)
import qualified Database.PostgreSQL.Simple as PG (execute, execute_)
import           Database.PostgreSQL.Simple.Copy
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


type ConnectedIO = ReaderT Connection IO


execute :: ToRow p => Query -> p -> ConnectedIO Int64
execute q p = ask >>= \conn -> liftIO $ PG.execute conn q p


execute_ :: Query -> ConnectedIO Int64
execute_ q = ask >>= \conn -> liftIO $ PG.execute_ conn q


runConnected :: ConnectedIO a -> Connection -> IO a
runConnected a conn = runReaderT a conn


-- | Produce unique internal names from a CSV header.
mkInternalNames :: [ColumnTitle] -> [InternalName]
mkInternalNames ns =
    Data.List.take (length ns) $
    map (\(tpl, num) -> fromString (tpl ++ show num)) $
    zip (repeat "field") ([1..] :: [Int])


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


getProgram :: Connection -> Int -> IO [Only Int]
getProgram conn sid =
    query conn
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
    map (\(FFAcc (CF c) _ _ _ _ _) -> fieldName c) vinFormatAccessors


makeProtoTable :: [InternalName] -> ConnectedIO Int64
makeProtoTable names =
    execute_ $ fromString $ concat
    [ "CREATE TEMPORARY TABLE vinnie_proto ("
    , intercalate "," $
      (map (\t -> (T.unpack t) ++ " text") names) ++
      [(T.unpack kid) ++ " serial"]
    , ");"
    ]


copyProtoStart :: Connection -> [InternalName] -> IO ()
copyProtoStart conn inames =
    copy conn
    [sql|
     COPY vinnie_proto (?)
     FROM STDIN (FORMAT CSV, HEADER 1, DELIMITER ';');
     |] (Only $ PT $ sqlCommas inames)


protoUpdate :: Query
protoUpdate = [sql|UPDATE vinnie_proto SET ? = ?;|]


protoUpdateWithFun :: InternalName
                   -> Text
                   -> [Text]
                   -> ConnectedIO Int64
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
                 -> ConnectedIO Int64
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
                -> ConnectedIO Int64
protoDictLookup iname dictTableName =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
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
protoPartnerLookup :: InternalName
                   -> ConnectedIO Int64
protoPartnerLookup iname =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=null WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT DISTINCT
      lower(trim(both ' ' from (unnest(ARRAY[name, code]))))
      FROM "?");

     WITH dict AS
     (SELECT DISTINCT ON (label) id AS did,
      lower(trim(both ' ' from (unnest(ARRAY[name, code])))) AS label
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
          partnerTable = PT $ tableName $ (modelInfo :: ModelInfo Partner)


-- | Replace subprogram label references with subprogram ids. Only
-- children of a provided program are selected. If no reference found,
-- default to a provided subprogram .
protoSubprogramLookup :: Int
                      -- ^ Program id.
                      -> Maybe Int
                      -- ^ Default subprogram id.
                      -> InternalName
                      -> ConnectedIO Int64
protoSubprogramLookup pid sid iname =
    execute
    [sql|
     UPDATE vinnie_proto SET ?=? WHERE lower(trim(both ' ' from ?)) NOT IN
     (SELECT
      lower(trim(both ' ' from s.label))
      FROM "?" s, "?" p WHERE s.parent = p.id AND p.id = ?);

     WITH dict AS
     (SELECT s.id AS did,
      lower(trim(both ' ' from s.label)) AS label
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
installFunctions :: ConnectedIO Int64
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
makeQueueTable :: ConnectedIO Int64
makeQueueTable =
    execute
    [sql|
     CREATE TEMPORARY TABLE vinnie_queue
     AS (SELECT * FROM "?" WHERE 'f');
     ALTER TABLE vinnie_queue ADD COLUMN errors text[];
     |] (Only contractTable)


-- | Set committer and subprogram (if not previously set) for
-- contracts in queue.
setSpecialDefaults :: Int -> Maybe Int -> ConnectedIO Int64
setSpecialDefaults cid sid =
    execute
    [sql|
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
              -> ConnectedIO Int64
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


-- | Add error to every row where a provided field is empty (used to
-- mark empty required fields). Parameters: error message, field name.
markEmptyRequired :: Query
markEmptyRequired =
    [sql|
     UPDATE vinnie_queue SET errors = errors || ARRAY[?]
     WHERE coalesce(length(lower(trim(both ' ' from (?::text)))) < 1, true);
     |]


-- | Delete all rows from the queue which are already present in
-- Contract table.
deleteDupes :: ConnectedIO Int64
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
transferContracts :: ConnectedIO Int64
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
              | NoSubprogram
                deriving Show

instance ToField RowError where
    toField (EmptyRequired t) =
        toField $ T.concat ["Обязательное поле «", t, "» не распознано"]
    toField NoSubprogram =
        toField $ T.concat ["Подпрограмма не распознана"]
