{-|

Main VIN import actions.

TODO Give an outline of how it works (pristine, proto, queue tables).

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.VIN
    ( doImport
    , Options(..)
    , ImportError(..)
    )

where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Error as E
import           Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import           Data.Conduit
import           Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.List as CL
import qualified Data.CSV.Conduit as CSV

import           Data.Int
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map

import           Data.String (fromString)
import           Data.Text (Text, toCaseFold)

import           Database.PostgreSQL.Simple (Only(..))
import           Database.PostgreSQL.Simple.Copy

import           System.IO

import           Data.Model
import           Data.Model.Patch as Patch
import           Carma.Model.VinFormat

import           Carma.VIN.Base
import           Carma.VIN.SQL


-- | Perform VIN file import, write report.
--
-- Return how many new rows were loaded and how many erroneous rows
-- occured.
doImport :: Options -> IO (Either ImportError (Int64, Int64))
doImport opts = runImport vinImport opts


getOption :: (Options -> a) -> Import a
getOption proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwError err


-- | Main VIN file import action.
--
-- TODO Track import state.
vinImport :: Import (Int64, Int64)
vinImport = do
  vf <- asks vinFormat
  (pid, sid) <- (,) <$> (getOption program) <*> (getOption subprogram)
  input <- getOption infile

  -- Figure out program id if only subprogram is provided.
  (pid', sid') <-
      case (pid, sid) of
        (Nothing, Nothing) -> throwError NoTarget
        (Just p,  Nothing) -> return (p, sid)
        (_     ,  Just s)  -> do
            res <- getProgram s
            case res of
              [Only p] -> return (p, Just s)
              _        -> throwError NoTarget

  -- Read head row to find out original column order
  hr <- liftIO $ (runResourceT $
        sourceIOHandle (skipBomInputHandle input) $=
        CSV.intoCSV csvSettings $$
        CL.head :: IO (Maybe (CSV.Row Text)))

  case hr of
    Nothing -> throwError NoHeader
    Just headRow -> do
        -- TODO Error on duplicate loadable columns
        mapping <- processTitles vf headRow
        -- Fail if a specific subprogram is not set and it's not
        -- loadable either. We check against the actual set of file
        -- columns, not the format.
        when (not $ (isJust sid') ||
              (hasSubprogram vf $ map ffa $ snd mapping)) $
             throwError NoTarget
        process (pid', sid') mapping


type FFA = FormatFieldAccessor VinFormat
type C a = Patch a


-- | Default settings for VIN list CSV files: semicolon-separated
-- fields, quoted. Must match those used in "Carma.VIN.SQL" COPY
-- commands.
csvSettings :: CSV.CSVSettings
csvSettings = CSV.CSVSettings ';' (Just '"')


bom :: B8.ByteString
bom = B8.pack ['\xef', '\xbb', '\xbf']


-- | Provide a read-only handle to a file, skipping first 3 bytes if
-- UTF-8 BOM is present (otherwise, read from the beginning).
skipBomInputHandle :: FilePath -> IO Handle
skipBomInputHandle fileName = do
  h <- openFile fileName ReadMode
  eof <- hIsEOF h
  when (not eof) $ do
    hSetEncoding h char8
    c <- hLookAhead h
    when (c == '\xef') $ hSeek h AbsoluteSeek 3
    hSetEncoding h utf8
  return h


-- | Get loadable fields of a format.
loadable :: C VinFormat -> [FFA] -> [FFA]
loadable vf =
    filter (\(FFAcc _ _ l _ _ _) -> Patch.get' vf l)


-- | Check if a subprogram field is loadable.
hasSubprogram :: C VinFormat -> [FFA] -> Bool
hasSubprogram vf ffas =
    any (\case
         (FFAcc _ SSubprogram _ _ _ _) -> True
         _                             -> False) $
    loadable vf ffas


isRequired :: C VinFormat -> FFA -> Bool
isRequired vf (FFAcc _ _ _ r _ _) = Patch.get' vf r


ffaFieldName :: FFA -> ContractFieldName
ffaFieldName (FFAcc (FA c) _ _ _ _ _) = CN $ fieldName c


-- | Binds internal names and format accessors.
data FFMapper =
    FM { internal :: InternalName
       , ffa      :: FFA
       -- ^ Format smart accessor.
       , concats  :: (Maybe [InternalName])
       -- ^ Columns to be concatenated to this field.
       }


-- | Scan CSV header and VinFormat, producing field mappings.
--
-- Return:
--
-- 1. An ordered mapping between CSV columns (header row) and internal
-- field names. All fields of CSV are present in this mapping, even
-- those not in the format.
--
-- 2. List of CSV columns which matched any of format fields, wrapped
-- in 'FFMapper'. Matching is case-insensitive. Internal name of any
-- such column is also present in the first mapping.
--
-- Yield error if the format is broken or the header misses required
-- columns.
processTitles :: C VinFormat
              -> [ColumnTitle]
              -- ^ CSV columns.
              -> Import ([(ColumnTitle, InternalName)], [FFMapper])
processTitles vf csvHeader =
    (\l -> (interMap, catMaybes l)) <$>
    (mapM
     (\f@(FFAcc (FA c) _ _ req _ _) ->
      let
          titles :: [ColumnTitle]
          titles = ffaTitles f vf
          errIfRequired e = if Patch.get' vf req
                            then throwError e
                            else return $ Nothing
      in
        case titles of
          -- Fail when a required column lacks proper titles
          [] -> errIfRequired $ NoTitle (fieldDesc c)
          _  ->
              case (filter (not . (flip elem iCsvHeader) . toCaseFold) titles,
                    titles) of
                -- Single-column field
                ([], [t])    -> return $ Just $
                                FM (interName t) f Nothing
                -- Multi-column
                ([], (t:ts)) -> return $ Just $
                                FM (interName t) f (Just $ map interName ts)
                -- Fail when a required column is missing
                (m, _) -> errIfRequired $ NoColumn (fieldDesc c) m) $
     loadable vf vinFormatAccessors)
        where
          iCsvHeader = map toCaseFold csvHeader
          interMap = zip csvHeader $ mkInternalNames csvHeader
          -- Unordered interMap with case-folded keys
          foldedInterMap =
              (Map.fromList $
               map (\(k, v) -> (toCaseFold k, v)) interMap)
          -- Internal name of a CSV column (case-insensitive lookup)
          interName t = foldedInterMap Map.! (toCaseFold t)


-- | Perform VIN file import using the provided mapping.
process :: (Int, Maybe Int)
        -- ^ Program & subprogram ids.
        -> ([(ColumnTitle, InternalName)], [FFMapper])
        -- ^ Total column mapping and loadable fields mapping.
        -> Import (Int64, Int64)
process psid mapping = do
  vf   <- asks vinFormat
  uid  <- getOption committer

  -- Load CSV data into pristine and proto tables, which match CSV
  -- file schema
  let (columnTitles, internalNames) = unzip $ fst mapping
      contractNames = map (ffaFieldName . ffa) $ snd mapping
  createCSVTables internalNames contractNames
  conn  <- asks connection
  input <- getOption infile
  copyPristineStart internalNames
  liftIO $ do
    BL.readFile input >>= mapM_ (putCopyData conn) . BL.toChunks
    putCopyEnd conn

  -- Load pristine data to proto table, which matches loadable subset
  -- of Contract
  let protoMapping :: [(InternalName, ContractFieldName)]
      protoMapping =
          zip (map internal $ snd mapping) contractNames
  pristineToProto protoMapping

  installFunctions >> createQueueTable

  -- Process proto table
  let (protoActions, transferChunks) =
          unzip $ map (processField psid) $ snd mapping
  sequence_ protoActions

  -- By now all values in proto are either suitable for Contract or
  -- null. Queue table has only typed Contract fields.
  protoToQueue transferChunks

  -- Set committer and subprogram. Note that if subprogram was not
  -- recognized in a file row, it will be set to the subprogram
  -- specified in import options.
  --
  -- TODO Probably the behavior should be different if subprogram is
  -- loadable from file and required.
  setSpecialDefaults uid (snd psid)

  forM_ (vinFormatAccessors) $
        (\(FFAcc (FA c) _ loadAcc reqAcc defAcc _) ->
             let
                 fn = fieldName c
                 -- TODO Might use CSV titles here instead of Contract
                 -- field name
                 fd = fieldDesc c
                 dv = Patch.get' vf defAcc
             in
               when (Patch.get' vf loadAcc) $ do
                 -- Write errors for empty required loadable
                 -- columns. Loadable required field with a
                 -- default value (when there's any) is marked
                 -- as missing when empty.
                 when (Patch.get' vf reqAcc) $
                      void $ execute markEmptyRequired (EmptyRequired fd, PT fn)
                 -- Set default values.
                 void $ execute setQueueDefaults (PT fn, dv, PT fn))
  markMissingIdentifiers

  -- Finally, write new contracts to live table
  loaded <- deleteDupes >> transferContracts

  -- Count errors and write error report if there're any
  errors <- countErrors

  when (errors > 0) $ do
       output <- getOption outfile

       copyReportStart internalNames
       liftIO $ void $ do
         BS.writeFile output bom
         -- Write report header, adding errors column title if not present
         runResourceT $ yield (nub $ columnTitles ++ [errorsTitle]) $=
                        CSV.fromCSV csvSettings $$
                        sinkIOHandle (openFile output AppendMode)

         -- Write COPY FROM data to outfile
         fix $ \next ->
             getCopyData conn >>= \case
                         CopyOutRow s ->
                             BS.appendFile output s >> next
                         CopyOutDone n -> return n
  return (loaded, errors)


pass :: Monad m => m ()
pass = return ()


-- | Produce an action for proto processing and bits for
-- proto-to-queue transfer (@("field23::int", "mileage")@) of a field.
processField :: (Int, Maybe Int)
             -- ^ Program & subprogram ids.
             -> FFMapper
             -> (Import (), (Text, ContractFieldName))
processField (pid, sid) (FM iname (FFAcc (FA c) stag _ _ defAcc _) cols) =
    case stag of
      SRaw -> (pass, (sqlCast cn "text"))
      SNumber ->
          ( void $ protoUpdateWithFun cn
            "regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (sqlCast cn "int"))
      SVIN ->
          ( void $ protoCheckRegexp iname cn
            "^[0-9a-hj-npr-z]{17}$"
          , (sqlCast cn "text"))
      SEmail ->
          ( void $ protoCheckRegexp iname cn
            "^[\\w\\+\\.\\-]+@[\\w\\+\\.\\-]+\\.\\w+$"
          , (sqlCast cn "text"))
      SPlate ->
          ( void $ protoCheckRegexp iname cn $ fromString $
            "^[АВЕКМНОРСТУХавекмнорстух]\\d{3}" ++
            "[АВЕКМНОРСТУХавекмнорстух]{2}\\d{2,3}$"
          , (sqlCast cn "text"))
      SYear ->
          ( void $ protoCheckRegexp iname cn $ fromString $
            "^[12][09][0-9]{2}$"
          , (sqlCast cn "int2"))
      SPhone ->
          ( void $ protoUpdateWithFun cn
            "'+'||regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (sqlCast cn "text"))
      SName ->
          ( case cols of
              Just l@(_:_) -> void $ protoUpdateWithFun cn "concat_ws"
                              ([joinSymbol, iname] ++ l)
              _            -> pass
          , (sqlCast cn "text"))
      SDate ->
          -- Delete all malformed dates
          ( void $ protoUpdateWithFun cn "pg_temp.dateordead" [cn']
          , (sqlCast cn "date"))
      SDict ->
          ( -- Try to recognize references to dictionary elements
            protoDictLookup iname cn (identModelName defAcc) >>
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , (sqlCast cn "int"))
      SSubprogram ->
          ( -- Try to recognize references to subprograms
            protoSubprogramLookup pid sid iname cn >>
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , (sqlCast cn "int"))
      SDealer ->
          ( -- Try to recognize partner names/codes from all columns
            -- (latter ones are preferred)
            protoPartnerCleanup allNames cn >>
            (forM_ allNames
             (\n -> do
                protoPartnerLookup n cn)) >>
            -- Delete if no matches found
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , (sqlCast cn "int"))
      where
        cn :: ContractFieldName
        cn = CN cn'
        cn' :: Text
        cn' = fieldName c
        allNames = [iname] ++ fromMaybe [] cols
