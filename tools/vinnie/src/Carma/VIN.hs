{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.VIN
    ( vinImport
    )

where

import           Control.Monad
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

import           Data.Functor
import           Data.Maybe

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Typeable
import           Data.Vector as Vector (Vector, toList)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Copy

import           System.IO

import           Data.Model
import           Data.Model.Patch as Patch
import           Carma.Model.VinFormat
import           Carma.Model.LegalForm

import           Carma.VIN.Base
import           Carma.VIN.SQL


getOption :: (Options -> a) -> Import a
getOption proj = lift $ lift $ asks proj


throwError :: ImportError -> Import a
throwError err = lift $ E.throwError err


type FFA = FormatFieldAccessor VinFormat
type C a = Patch a


-- | Default settings for VIN list CSV files: semicolon-separated
-- fields, quoted.
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
loadable vf = filter (\(FFAcc _ _ l _ _ _) -> Patch.get' vf l)


-- | Binds internal names and format accessors.
data FFMapper =
    FM { internal :: InternalName
       , ffa      :: FFA
       -- ^ Format smart accessor.
       , concats  :: (Maybe [InternalName])
       -- ^ A list of fields to be concatenated to this field.
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


tmpDir :: FilePath
tmpDir = "/tmp"


process :: Options
        -> ImportContext
        -> ([(ColumnTitle, InternalName)], [FFMapper])
        -- ^ Total column mapping and loadable fields mapping.
        -> Connection
        -> IO ()
process options context mapping conn = do
  let input           = infile options
      uid             = committer options
      vf              = vinFormat context
      interMap        = fst mapping
      internalHeadRow = map snd interMap

  -- Read CSV into proto table
  execute_ conn $ makeProtoTable internalHeadRow
  execute_ conn installFunctions
  copy_ conn copyProtoStart
  BL.readFile input >>= mapM_ (putCopyData conn) . BL.toChunks
  res <- putCopyEnd conn

  execute_ conn makeQueueTable

  -- Process in proto table, which matches CSV columns.
  let (protoActions, transferChunks :: [(Text, Text)]) =
          unzip $ map (processField conn) $ snd mapping
  sequence_ protoActions

  -- Transfer to queue table, which matches Contract schema.
  execute conn transferQueue $
              (\(f, s) -> (PT $ sqlCommas f, PT $ sqlCommas s)) $
              unzip transferChunks

  forM_ (vinFormatAccessors) $
        (\f@(FFAcc (CF c) _ _ reqAcc defAcc _) ->
             let
                 fn = fieldName c
                 -- TODO Might use CSV titles here instead of Contract
                 -- field name
                 fd = fieldDesc c
                 dv = Patch.get' vf defAcc
             in do
               -- Set default values
               execute conn setQueueDefaults (PT fn, dv, PT fn)
               -- Write errors for empty required columns.
               --
               -- TODO Probably non-loadable required fields must be
               -- ignored here.
               when (Patch.get' vf reqAcc) $
                    execute conn markEmptyRequired (EmptyRequired fd, PT fn) >>
                    pass)

  execute conn setCommitter (Only uid)

  let contractNames =
          (map (\(FFAcc (CF c) _ _ _ _ _) ->
                fieldName c) vinFormatAccessors)
      pt2 s = (PT $ sqlCommas s, PT $ sqlCommas s)

  execute conn deleteDupes $ pt2 contractNames
  execute conn transferContracts $ pt2 $ "committer":contractNames

  pass


pass :: IO ()
pass = return ()


-- | Produce actions for proto processing and bits for proto-to-queue
-- transfer.
processField :: Connection -> FFMapper -> (IO (), (Text, Text))
processField conn (FM iname (FFAcc (CF c) stag _ _ defAcc _) cols) =
    case stag of
      SNumber ->
          ( void $ protoUpdateWithFun conn iname 
            "regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (fn, sqlCast iname "int"))
      SPhone ->
          ( void $ protoUpdateWithFun conn iname
            "'+'||regexp_replace" [iname, "'\\D'", "''", "'g'"]
          , (fn, sqlCast iname "text"))
      SName ->
          ( case cols of
              Just l@(_:_) -> void $ protoUpdateWithFun conn iname "concat_ws"
                              ([joinSymbol, iname] ++ l)
              _            -> pass
          , (fn, sqlCast iname "text"))
      SRaw -> (pass, (fn, sqlCast iname "text"))
      SDate ->
          -- Delete all malformed dates
          ( void $ protoUpdateWithFun conn iname "pg_temp.dateordead" [iname]
          , (fn, sqlCast iname "date"))
      SDict ->
          ( -- Try to recognize references to dictionary elements
            protoDictLookup conn iname (identModelName defAcc) >>
            protoUpdateWithFun conn iname "pg_temp.numordead" [iname] >>
            pass
          , (fn, sqlCast iname "int"))
      SDealer ->
          ( -- Try to recognize partner names/codes in all columns
            (forM_ allNames
             (\n -> do
                protoPartnerLookup conn n
                protoUpdateWithFun conn n "pg_temp.numordead" [n])) >>
            -- Pick the first match
            protoUpdateWithFun conn iname "coalesce" allNames >>
            pass
          , (fn, sqlCast iname "int"))
      where
        fn = fieldName c
        allNames = [iname] ++ fromMaybe [] cols


vinImport :: Import ()
vinImport = do
  vf <- asks vinFormat
  conn <- asks connection
  input <- getOption infile
  output <- getOption outfile

  -- Read head row to find out original column order
  hr <- liftIO $ (runResourceT $
        sourceFile input $=
        CSV.intoCSV csvSettings $$
        CL.head :: IO (Maybe (CSV.Row Text)))

  case hr of
    Nothing -> throwError NoHeader
    Just headRow -> do
        -- TODO Error on duplicate loadable columns
        mapping <- processTitles vf headRow
        ctx <- ask
        opts <- lift $ lift $ ask
        liftIO $ process opts ctx mapping conn
