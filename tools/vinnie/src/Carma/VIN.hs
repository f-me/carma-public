{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.VIN
    ( vinImport
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

import           Data.Conduit
import           Data.Conduit.Binary hiding (mapM_)
import qualified Data.Conduit.List as CL
import qualified Data.CSV.Conduit as CSV

import           Data.Maybe

import           Data.List
import qualified Data.Map as Map

import           Data.String (fromString)
import           Data.Text (Text)

import           Database.PostgreSQL.Simple (Only(..))
import           Database.PostgreSQL.Simple.Copy

import           System.IO

import           Data.Model
import           Data.Model.Patch as Patch
import           Carma.Model.VinFormat

import           Carma.VIN.Base
import           Carma.VIN.SQL


getOption :: (Options -> a) -> Import a
getOption proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwError err


type FFA = FormatFieldAccessor VinFormat
type C a = Patch a


-- | Default settings for VIN list CSV files: semicolon-separated
-- fields, quoted. Must match those used in SQL COPY commands.
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
-- 1. A mapping between CSV columns (header row) and internal field
-- names.
--
-- 2. List of CSV columns which matched any of format fields, wrapped
-- in 'FFMapper'. Internal name of any such column is also present in
-- the first mapping.
--
-- Yield error if the format is broken or the header misses required
-- columns.
processTitles :: C VinFormat
              -> [ColumnTitle]
              -- ^ CSV columns.
              -> Import ([(ColumnTitle, InternalName)], [FFMapper])
processTitles vf csvHeader =
    (\l -> (interMapOrd, catMaybes l)) <$>
    (mapM
     (\f@(FFAcc (CF c) _ _ req _ _) ->
      let
          titles :: [ColumnTitle]
          titles = ffaTitles f vf
          errIfRequired e = if Patch.get' vf req
                            then throwError e
                            else return $ Nothing
      in
        case titles of
          -- Fail when a required column lacks proper titles
          []     -> errIfRequired $ NoTitle (fieldDesc c)
          notNil ->
              case (filter (\x -> not $ x `elem` csvHeader) notNil, notNil) of
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
          interMapOrd = zip csvHeader $ mkInternalNames csvHeader
          interMap = Map.fromList interMapOrd
          interName t = interMap Map.! t


process :: (Int, Maybe Int)
        -- ^ Program & subprogram ids.
        -> ([(ColumnTitle, InternalName)], [FFMapper])
        -- ^ Total column mapping and loadable fields mapping.
        -> Import ()
process psid mapping = do
  vf   <- asks vinFormat
  uid  <- getOption committer

  let (columnTitles, internalNames) = unzip $ fst mapping

  makeProtoTables internalNames >> installFunctions >> makeQueueTable
  getOption infile >>= flip copyProto internalNames

  -- Process in proto table, which matches CSV columns.
  let (protoActions, transferChunks) =
          unzip $ map (processField vf psid) $ snd mapping
  sequence_ protoActions

  uncurry transferQueue (unzip transferChunks)

  -- Set committer and subprogram. Note that if subprogram was not
  -- recognized in a file row, it will be set to the subprogram
  -- specified in import options.
  --
  -- TODO Probably the behavior should be different if subprogram is
  -- loadable from file and required.
  setSpecialDefaults uid (snd psid)

  forM_ (vinFormatAccessors) $
        (\(FFAcc (CF c) _ _ reqAcc defAcc _) ->
             let
                 fn = fieldName c
                 -- TODO Might use CSV titles here instead of Contract
                 -- field name
                 fd = fieldDesc c
                 dv = Patch.get' vf defAcc
             in do
               -- Set default values
               execute setQueueDefaults (PT fn, dv, PT fn)
               -- Write errors for empty required columns.
               --
               -- TODO Probably non-loadable required fields must be
               -- ignored here.
               when (Patch.get' vf reqAcc) $
                    execute markEmptyRequired (EmptyRequired fd, PT fn) >>
                    pass)

  -- Finally, write new contracts to live table
  deleteDupes >> transferContracts

  conn <- asks connection
  output <- getOption outfile

  copyReportStart internalNames
  liftIO $ do
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
  pass


pass :: Monad m => m ()
pass = return ()


-- | Produce an action for proto processing and bits for
-- proto-to-queue transfer (@("field23::int", "mileage")@) of a given
-- field.
processField :: C VinFormat
             -> (Int, Maybe Int)
             -- ^ Program & subprogram ids.
             -> FFMapper
             -> (Import (), (Text, Text))
processField vf (pid, sid) (FM iname f@(FFAcc (CF c) stag _ _ defAcc _) cols) =
    case stag of
      SRaw -> (pass, (sqlCast iname "text", fn))
      SNumber ->
          ( void $ protoUpdateWithFun iname
            "regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (sqlCast iname "int", fn))
      -- TODO Perhaps mismatched regexp fields should always fail a
      -- row, not only when the field is required
      SVIN ->
          ( when (isRequired vf f) $
                 void $ protoCheckRegexp iname
                          "^[0-9a-hj-npr-z]{17}$"
          , (sqlCast iname "text", fn))
      SEmail ->
          ( when (isRequired vf f) $
                 void $ protoCheckRegexp iname
                          "^[\\w\\+\\.\\-]+@[\\w\\+\\.\\-]+\\.\\w+$"
          , (sqlCast iname "text", fn))
      SPlate ->
          ( when (isRequired vf f) $
                 void $ protoCheckRegexp iname $ fromString $
                          "^[АВЕКМНОРСТУХавекмнорстух]\\d{3}" ++
                          "[АВЕКМНОРСТУХавекмнорстух]{2}\\d{2,3}$"
          , (sqlCast iname "text", fn))
      SPhone ->
          ( void $ protoUpdateWithFun iname
            "'+'||regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (sqlCast iname "text", fn))
      SName ->
          ( case cols of
              Just l@(_:_) -> void $ protoUpdateWithFun iname "concat_ws"
                              ([joinSymbol, iname] ++ l)
              _            -> pass
          , (sqlCast iname "text", fn))
      SDate ->
          -- Delete all malformed dates
          ( void $ protoUpdateWithFun iname "pg_temp.dateordead" [iname]
          , (sqlCast iname "date", fn))
      SDict ->
          ( -- Try to recognize references to dictionary elements
            protoDictLookup iname (identModelName defAcc) >>
            protoUpdateWithFun iname "pg_temp.numordead" [iname] >>
            pass
          , (sqlCast iname "int", fn))
      SSubprogram ->
          ( -- Try to recognize references to subprograms
            protoSubprogramLookup pid sid iname >>
            protoUpdateWithFun iname "pg_temp.numordead" [iname] >>
            pass
          , (sqlCast iname "int", fn))
      SDealer ->
          ( -- Try to recognize partner names/codes in all columns
            (forM_ allNames
             (\n -> do
                protoPartnerLookup n
                protoUpdateWithFun n "pg_temp.numordead" [n])) >>
            -- Pick the first match
            protoUpdateWithFun iname "coalesce" allNames >>
            pass
          , (sqlCast iname "int", fn))
      where
        fn = fieldName c
        allNames = [iname] ++ fromMaybe [] cols


-- | TODO Track import state.
vinImport :: Import ()
vinImport = do
  vf <- asks vinFormat
  conn <- asks connection
  (pid, sid) <- (,) <$> (getOption program) <*> (getOption subprogram)
  input <- getOption infile

  -- Figure out program id if only subprogram is provided.
  (pid', sid') <-
      case (pid, sid) of
        (Nothing, Nothing) -> throwError NoTarget
        (Just p,  Nothing) -> return (p, sid)
        (_     ,  Just s)  -> do
            res <- liftIO $ getProgram conn s
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
