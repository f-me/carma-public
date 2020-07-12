{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Snaplet.Search.Contract where

import           Control.Monad.Fix
import           Control.Monad.IO.Class

import           Data.Aeson

import           Data.ByteString as BS
import           Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.String (fromString)

import           Data.Text (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T

import           System.IO
import           System.Directory
import           Text.Printf

import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.Copy as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           Snap.Core
import           Snap.Snaplet.PostgresqlSimple (liftPG')

import           Data.Model
import           Data.Model.Patch as Patch
import           Data.Model.Utils.PostgreSQL.InterpolationHelpers

import           Carma.Model.Contract
import qualified Carma.Model.Usermeta as Usermeta

import           Snaplet.Auth.PGUsers
import           Snaplet.Search.Types
import           Snaplet.Search.Utils
import           Carma.Utils.Snap (withLens)


contractSearch :: SearchHandler b
              (Either String
               (SearchResult
                (Patch Contract :. ())))
contractSearch = defaultSearch contractSearchParams mkQuery


mkQuery :: forall t.MkSelect t => t -> Text -> Int -> Int -> String -> Query
mkQuery _ predicate lim offset ord
  = fromString $ printf
      (  "    select %s"
      ++ "     from \"Contract\""
      ++ "     where (%s and dixi) %s limit %i offset %i;"
      )
      (T.unpack $ mkSel (undefined :: t))
      (T.unpack predicate) ord lim offset


-- | Read contract search parameters as JSON from @q@ query parameter
-- and serve search results as CSV.
contractCSV :: (Text -> Text) -> SearchHandler b ()
contractCSV t = do
  q <- getParam "q"
  let args' = decode =<< (fromStrict <$> q)
      bom :: B8.ByteString
      bom = B8.pack ['\xef', '\xbb', '\xbf']
      -- COPY TO STDOUT query with CSV data, 3 (plain text) arguments:
      -- fields list, predicates, ordering
      csvQuery :: Query
      csvQuery
          = [sql|
             COPY (SELECT ? FROM "Contract_csv" WHERE (? and dixi) ?)
             TO STDOUT (FORMAT CSV, FORCE_QUOTE *, DELIMITER ';')
             |]
      -- Convert list [foo, bar, ..] of fields to [fooExternal,
      -- barExternal, ..] (see "Contract_csv" view) and concatenate it
      -- with commas
      fieldList :: [Text] -> Text
      fieldList fs = T.intercalate "," $
                     Prelude.map (`T.append` "External") fs
      cmi :: ModelInfo Contract
      cmi = modelInfo
      -- Non-standard view is used while predicates are provided for
      -- Contract model
      fixTable = T.replace (tableName cmi) "Contract_csv"
      fieldNameToDesc :: ModelInfo m -> Text -> Maybe Text
      fieldNameToDesc mi name =
          case HM.lookup name (modelFieldsMap mi) of
            Just f -> Just $ fd_desc f
            _ | name == "dealercode" -> Just "Код ДЦ"
              | otherwise -> Nothing
  case args' of
    Nothing -> error "Could not read parameters"
    Just args -> do
      modifyResponse $ setContentType "text/csv"
      (fp, fh) <- liftIO $
                  getTemporaryDirectory >>=
                  flip openTempFile "portal.csv"
      withLens db $ liftPG' $ \c -> do
        BS.hPut fh bom
        prms <- renderPrms c (predicates args) contractSearchParams
        -- Prepare query and request CSV from Postgres
        case (resultFields args, prms) of
          (Just rf', Right p) -> do
            let rf = rf' ++ ["dealercode"]
            let header = T.intercalate ";" $
                         Prelude.map ((\f -> T.concat ["\"", f, "\""]) .
                                      fromMaybe (error "Unknown field") .
                                      fieldNameToDesc cmi) rf
            T.hPutStrLn fh $! header
            PG.copy c csvQuery
                     -- TODO Check field permissions in resultFields
                     (PT $ fieldList rf,
                      PT $ fixTable (t p),
                      PT $ fixTable $ T.pack $ renderOrder $ sorts args)
            -- Write selected rows to response
            fix $ \next ->
                getCopyData c >>= \case
                            CopyOutRow row ->
                                B8.hPut fh row >> next
                            CopyOutDone _ -> return ()
          _ -> error "Error reading predicates or header fields"
      liftIO $ hClose fh
      -- TODO Using withTempFile instead breaks sendFile, so we have
      -- to keep the temporary file even after the handler finishes.
      sendFile fp


-- | Identical to 'contractSearch' but with 'portalQuery' applied.
portalSearch :: SearchHandler b
                (Either String
                 (SearchResult
                  (Patch Contract :. ())))
portalSearch =
    portalHandler searchHandler
        where
          searchHandler t =
            defaultSearch contractSearchParams mkQuery'
                where
                  mkQuery' s p = mkQuery s $ t p


-- | Feed 'portalQuery' result to a handler.
portalHandler :: ((Text -> Text) -> SearchHandler b t)
              -> SearchHandler b t
portalHandler f = do
  Just user <- currentUserMeta
  let Just (Ident uid) = Patch.get user Usermeta.ident
  let Just isDealer = Patch.get user Usermeta.isDealer
  let commPred = printf " AND committer = %i" uid
  -- Build function which adds an SQL predicate to select only
  -- contracts created by current user if isDealer flag is set.
  f $ \s -> if isDealer
            then T.append s $ T.pack commPred
            else s
