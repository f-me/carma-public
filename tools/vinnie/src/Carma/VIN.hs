{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Main VIN import actions.

TODO Give an outline of how it works (pristine, proto, queue tables).

-}

module Carma.VIN
    ( doImport
    , Options(..)
    , ImportResult(..)
    , ImportError(..)
    )

where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

import           Data.Conduit
import           Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit (runResourceT)
import qualified Data.CSV.Conduit as CSV

import           Data.List
import           Data.Maybe
import qualified Data.Map as Map

import           Data.String (fromString)
import           Data.Text (Text, toCaseFold, snoc)
import           Data.Text.Encoding
import qualified Data.Text.ICU.Convert as ICU

import           Database.PostgreSQL.Simple (Connection, Only(..))
import           Database.PostgreSQL.Simple.Copy
import           Database.PostgreSQL.Simple.Transaction

import           System.FilePath
import           System.IO

import           Data.Model
import           Data.Model.Patch as Patch hiding (delete)
import           Carma.Model.VinFormat

import           Carma.VIN.Base
import           Carma.VIN.SQL


-- | Perform VIN file import, write report.
doImport :: Options -> Connection -> IO (Either ImportError ImportResult)
doImport = runImport vinImport


getOption :: (Options -> a) -> Import a
getOption proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwE err


-- | Main VIN file import action.
--
-- TODO Track import state.
vinImport :: Import ImportResult
vinImport = do
  vf <- asks vinFormat
  (pid, sid) <- (,) <$> getOption program <*> getOption subprogram
  input <- getOption infile

  -- Figure out program id if only subprogram is provided.
  (pid', sid') <-
      case (pid, sid) of
        (Nothing, Nothing) -> throwError NoTargetSubprogram
        (Just p,  Nothing) -> return (p, sid)
        (_     ,  Just s)  -> do
            res <- getProgram s
            case res of
              [Only p] -> return (p, Just s)
              _        -> throwError NoTargetSubprogram

  -- Read head row to find out original column order
  (hr, enc) <- readHeaderAndEncoding input

  case hr of
    Nothing -> throwError NoHeader
    Just headRow -> do
        -- TODO Error on duplicate loadable columns
        mapping <- processTitles vf headRow
        -- Fail if a specific subprogram is not set and it's not
        -- loadable either. We check against the actual set of file
        -- columns, not the format.
        when (not $ isJust sid' ||
              hasSubprogram vf (map ffa $ snd mapping)) $
             throwError NoTargetSubprogram
        process (pid', sid') enc mapping


type FFA = FormatFieldAccessor VinFormat
type C a = FullPatch a


-- | Default settings for VIN list CSV files: semicolon-separated
-- fields, quoted. Must match those used in "Carma.VIN.SQL" COPY
-- commands.
csvSettings :: CSV.CSVSettings
csvSettings = CSV.CSVSettings ';' (Just '"')


bom :: B8.ByteString
bom = B8.pack ['\xef', '\xbb', '\xbf']


-- | Skip first 3 bytes if a bytestring starts with UTF-8 BOM
-- (otherwise, read from the beginning).
skipBom :: B8.ByteString -> B8.ByteString
skipBom bs | bom `B8.isPrefixOf` bs = B8.drop (B8.length bom) bs
           | otherwise              = bs


-- | Read CSV file header header, guess file encoding for Postgres
-- COPY. If not enough lines in the file, throw 'ImportError'.
-- Encoding recognition uses first two lines of the file.
readHeaderAndEncoding :: FilePath -> Import (Maybe (CSV.Row Text), String)
readHeaderAndEncoding fileName = do
  r <- liftIO $
       openFile fileName ReadMode >>=
       \h -> try ((,) <$> B8.hGetLine h <*> B8.hGetLine h)
  case r of
    Right (l1, l2) ->
        do
          (topText, enc) <-
              case (decodeUtf8' l1, decodeUtf8' l2) of
                (Right _, Right _) ->
                    return (decodeUtf8 $ skipBom l1, "UTF8")
                _ -> do
                  conv <- liftIO $ ICU.open "CP1251" Nothing
                  return (ICU.toUnicode conv l1, "WIN1251")
          header <- runResourceT $
                yield (topText `snoc` '\n') $=
                CSV.intoCSV csvSettings $$ CL.head
          return (header, enc)
    Left e -> throwError $ NotEnoughData e


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


ffaFieldName :: FFA -> ContractFieldName
ffaFieldName (FFAcc (FA c) _ _ _ _ _) = cfn c


-- | Binds internal names and format accessors.
data FFMapper =
    FM { internal :: InternalName
       , ffa      :: FFA
       -- ^ Format smart accessor.
       , _concats  :: Maybe [InternalName]
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
                            else return Nothing
      in
        case titles of
          -- Fail when a required column lacks proper titles
          [] -> errIfRequired $ NoTitle (fieldDesc c)
          _  ->
              case (filter (not . (`elem` iCsvHeader) . toCaseFold) titles,
                    titles) of
                -- Single-column field
                ([], [t])    -> return $ Just $
                                FM (interName t) f Nothing
                -- Multi-column
                ([], t:ts) -> return $ Just $
                                FM (interName t) f (Just $ map interName ts)
                -- Fail when a required column is missing
                (m, _) -> errIfRequired $ NoColumn (fieldDesc c) m) $
     loadable vf vinFormatAccessors)
        where
          iCsvHeader = map toCaseFold csvHeader
          interMap = zip csvHeader $ mkInternalNames csvHeader
          -- Unordered interMap with case-folded keys
          foldedInterMap =
              Map.fromList $
              map (\(k, v) -> (toCaseFold k, v)) interMap
          -- Internal name of a CSV column (case-insensitive lookup)
          interName t = foldedInterMap Map.! toCaseFold t


-- | Perform VIN file import using the provided mapping.
process :: (Int, Maybe Int)
        -- ^ Program & subprogram ids, obtained from import options.
        -> String
        -- ^ Input file encoding name.
        -> ([(ColumnTitle, InternalName)], [FFMapper])
        -- ^ Total column mapping and loadable fields mapping.
        -> Import ImportResult
process psid enc mapping = do
  vf   <- asks vinFormat
  uid  <- getOption committer

  -- Load CSV data into pristine and proto tables, which match CSV
  -- file schema
  let (columnTitles, internalNames) = unzip $ fst mapping
      contractNames = map (ffaFieldName . ffa) $ snd mapping
  createCSVTables internalNames contractNames
  conn  <- asks connection
  input <- getOption infile

  copyPristineStart enc internalNames
  res <- liftIO $ try $ do
           BL.readFile input >>= mapM_ (putCopyData conn) . BL.toChunks
           putCopyEnd conn
  total <- case res of
             Right r                 -> return r
             Left (_ :: IOException) -> throwError PGLoadingFailed

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

  forM_ vinFormatAccessors $
        \(FFAcc (FA c) _ loadAcc reqAcc defAcc _) ->
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
                 void $ execute setQueueDefaults (PT fn, dv, PT fn)

  arcVal <- getOption fromArc
  -- Set service field values. If the subprogram is loadable and
  -- was not recognized in a file row, it will be set to the
  -- subprogram specified in import options. However, if it is
  -- required, the corresponding file row has already been marked as
  -- erroneous on the previous step.
  setSpecialDefaults uid (snd psid) arcVal (takeFileName input)

  markMissingIdentifiers

  -- Finally, write new contracts to live table, omitting those
  -- already present and duplicate contracts in the queue
  let finalTransfer tries = do
        -- Prevent phantom reads when deleting duplicate contracts
        -- from the queue
        liftIO $ beginLevel Serializable conn
        ser <- liftIO $ try $
               deleteDupes conn >>
               deferConstraints conn >>
               transferContracts conn
        case ser of
          Right n -> liftIO (commit conn) >> return n
          Left e -> if isSerializationError e
                    then if tries > 0
                         then liftIO (rollback conn) >>
                              finalTransfer (tries - 1 :: Int)
                         else throwError SerializationFailed
                    else throw e
  loaded <- finalTransfer 10

  -- Count errors and write error report if there're any
  errors <- countErrors

  when (errors > 0) $ do
       output <- getOption outfile

       copyReportStart internalNames
       liftIO $ void $ do
         BS.writeFile output bom
         -- Write report header, adding errors column title if not present
         runResourceT $ yield (delete errorsTitle columnTitles ++
                               [errorsTitle]) $=
                        CSV.fromCSV csvSettings $$
                        sinkIOHandle (openFile output AppendMode)

         -- Write COPY FROM data to outfile
         fix $ \next ->
             getCopyData conn >>= \case
                         CopyOutRow s ->
                             BS.appendFile output s >> next
                         CopyOutDone n -> return n

  deleteQueueTable
  deleteCSVTables
  return $ ImportResult (total, loaded, errors)


pass :: Monad m => m ()
pass = return ()


-- | Produce an action for proto processing and bits for
-- proto-to-queue transfer (@("field23::int", "mileage")@) of a field.
processField :: (Int, Maybe Int)
             -- ^ Program & subprogram ids.
             -> FFMapper
             -> (Import (), (Text, ContractFieldName))
processField (pid, _) (FM iname (FFAcc (FA c) stag _ _ defAcc _) cols) =
    case stag of
      SRaw -> (pass, sqlCast cn "text")
      SNumber ->
          ( protoUpdateWithFun cn
            "regexp_replace" [iname, "'\\D'", "''", "'g'"] >>
            -- If no characters left, replace it with NULL, as an
            -- empty string cannot be cast to int
            protoNullizeEmptyStrings cn >>
            pass
          , sqlCast cn "int")
      SVIN ->
          -- We don't use protoUpdateWithFun here because it breaks
          -- encoding of function arguments
          ( protoTranslate iname
            "ЗАВЕКМНРСТУХавекмнрстух" "3ABEKMHPCTYXabekmhpctyx" cn >>
            protoCheckRegexp cn "^[0-9a-hj-npr-z]{17}$" >>
            pass
          , sqlCast cn "text")
      SEmail ->
          ( protoTransfer iname cn >>
            protoCheckRegexp cn
            "^[\\w\\+\\.\\-]+@[\\w\\+\\.\\-]+\\.\\w+$" >>
            pass
          , sqlCast cn "text")
      SPlate ->
          ( protoTransfer iname cn >>
            protoCheckRegexp cn (fromString $
             "^[АВЕКМНОРСТУХавекмнорстух]\\d{3}" ++
             "[АВЕКМНОРСТУХавекмнорстух]{2}\\d{2,3}$") >>
            pass
          , sqlCast cn "text")
      SYear ->
          ( protoTransfer iname cn >>
            protoCheckRegexp cn (fromString
             "^[12][09][0-9]{2}$") >>
            pass
          , sqlCast cn "int2")
      SPhone ->
          ( void $ protoUpdateWithFun cn
            "'+'||regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , sqlCast cn "text")
      SName ->
          ( case cols of
              Just l@(_:_) -> void $ protoUpdateWithFun cn "concat_ws"
                              ([joinSymbol, iname] ++ l)
              _            -> pass
          , sqlCast cn "text")
      SDate ->
          -- Delete all malformed dates
          ( void $ protoUpdateWithFun cn "pg_temp.dateordead" [cn']
          , sqlCast cn "date")
      SDict ->
          ( -- Try to recognize references to dictionary elements
            protoDictCleanup iname cn (identModelName defAcc) >>
            protoDictLookup iname cn (identModelName defAcc) >>
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , sqlCast cn "int")
      SSubprogram ->
          ( -- Try to recognize references to subprograms
            protoSubprogramLookup pid iname cn >>
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , sqlCast cn "int")
      SDealer ->
          ( -- Try to recognize partner names/codes from all columns
            -- (latter ones are preferred)
            protoPartnerCleanup allNames cn >>
            forM_ allNames
             (`protoPartnerLookup` cn) >>
            -- Delete if no matches found
            protoUpdateWithFun cn "pg_temp.numordead" [cn'] >>
            pass
          , sqlCast cn "int")
      where
        cn :: ContractFieldName
        cn = CN cn'
        cn' :: Text
        cn' = fieldName c
        allNames = iname : fromMaybe [] cols
