{-|

Basic types and monads of VIN import process.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carma.VIN.Base
    ( ColumnTitle
    , Import
    , Options(..)
    , ImportContext(..)
    , ImportResult(..)
    , ImportError(..)
    , runImport
    )

where

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader

import Data.Aeson as A
import Data.Data
import Data.Int
import Data.Text as T (Text, concat, pack)
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch     as Patch
import Data.Model.Patch.Sql as Patch

import Carma.Model.VinFormat  (VinFormat)


deriving instance Data ConnectInfo


type ColumnTitle = Text


data Options = Options
    { cInfo      :: ConnectInfo
    , infile     :: FilePath
    , outfile    :: FilePath
    , committer  :: Int
    , format     :: Int
    , program    :: Maybe Int
    -- ^ If not set, may be derived from subprogram.
    , subprogram :: Maybe Int
    -- ^ If subprogram not set, VIN file must provide a subprogram
    -- name in every row.
    , fromArc    :: Bool
    -- ^ Contracts are loaded from ARC.
    } deriving (Show, Data, Typeable)


data ImportContext = ImportContext
    { connection :: Connection
    , vinFormat  :: Patch VinFormat
    }


-- | Import result: total rows processed, loaded row, rejected rows.
newtype ImportResult = ImportResult (Int64, Int64, Int64)


-- | Critical VIN import errors which result in the whole process
-- being interrupted.
data ImportError = NoTarget
                 | NoHeader
                 | NoData IOException
                 | LoadingFailed
                 | UnknownVinFormat
                 | NoColumn Text [ColumnTitle]
                 -- ^ The file misses one or several columns for a
                 -- loadable required field.
                 | DuplicateColumn Text
                 -- ^ A loadable column is present more than once in
                 -- the file.
                 | NoTitle Text
                 -- ^ Loadable required field has empty column title.
                 deriving Show

instance ToJSON ImportError where
    toJSON t = A.String $ case t of
        NoTarget -> "Невозможно определить подпрограмму"
        NoHeader -> "В файле отсутствует корректный заголовок"
        NoData e -> T.concat ["Не удалось прочитать данные из файла ("
                             , (T.pack $ show e)
                             , ")"
                             ]
        LoadingFailed -> "Не удалось загрузить данные в PostgreSQL"
        UnknownVinFormat -> "Неизвестный формат"
        NoColumn v _ ->
            T.concat ["Отсутствует колонка обязательного поля «", v, "»"]
        DuplicateColumn v ->
            T.concat ["Повторяющаяся колонка «", v, "»"]
        NoTitle v ->
            T.concat ["Не задан заголовок обязательного поля «", v, "»"]


instance Error ImportError


-- | Base monad.
type Import =
    ReaderT ImportContext
    (ErrorT ImportError
     (ReaderT Options IO))


-- | Perform VIN import action using the provided options.
runImport :: Import a -> Options -> IO (Either ImportError a)
runImport act opts =
    flip runReaderT opts $ runErrorT $ do
      fid <- lift $ asks format
      -- Close connection when short-circuiting Import monad
      liftBaseOp (bracket
                  (connect $ cInfo opts)
                  (close)) $
                  \c -> do
                    vf <- liftIO $ Patch.read (Ident fid) c
                    case vf of
                      (vf':_) -> runReaderT act $ ImportContext c vf'
                      _       -> throwError UnknownVinFormat
