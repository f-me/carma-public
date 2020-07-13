{-|

Basic types and monads of VIN import process.

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.Aeson as A hiding (Options)
import Data.Int
import Data.Text as T (Text, concat, pack)
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch     as Patch
import Data.Model.Patch.Sql as Patch

import Carma.Model.VinFormat  (VinFormat)

type ColumnTitle = Text

data Options = Options
    { infile     :: FilePath
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
    } deriving (Show)


data ImportContext = ImportContext
    { connection :: Connection
    , vinFormat  :: FullPatch VinFormat
    }


-- | Import result: total rows processed, loaded row, rejected rows.
newtype ImportResult = ImportResult (Int64, Int64, Int64)
                       deriving Show


-- | Critical VIN import errors which result in the whole process
-- being interrupted.
data ImportError = NoTargetSubprogram
                 | NoHeader
                 | NotEnoughData IOException
                 | PGLoadingFailed
                 -- ^ COPY command failed.
                 | UnknownVinFormat
                 | NoColumn Text [ColumnTitle]
                 -- ^ The file misses one or several columns for a
                 -- loadable required field.
                 | DuplicateColumn Text
                 -- ^ A loadable column is present more than once in
                 -- the file.
                 | NoTitle Text
                 -- ^ Loadable required field has empty column title.
                 | SerializationFailed
                 -- ^ Failed to obtain a write lock on contract table.
                 deriving Show


instance ToJSON ImportError where
    toJSON t = A.String $ case t of
        NoTargetSubprogram -> "Невозможно определить подпрограмму"
        NoHeader -> "В файле отсутствует корректный заголовок"
        NotEnoughData e -> T.concat ["В файле недостаточно данных ("
                                    , T.pack $ show e
                                    , ")"
                                    ]
        PGLoadingFailed -> "Не удалось загрузить данные в PostgreSQL"
        UnknownVinFormat -> "Неизвестный формат"
        NoColumn v _ ->
            T.concat ["Отсутствует колонка обязательного поля «", v, "»"]
        DuplicateColumn v ->
            T.concat ["Повторяющаяся колонка «", v, "»"]
        NoTitle v ->
            T.concat ["Не задан заголовок обязательного поля «", v, "»"]
        SerializationFailed ->
            T.concat [ "Не удалось заблокировать таблицу контрактов "
                     , "(кто-то одновременно в неё пишет)"
                     ]


-- | Base monad.
type Import =
    ReaderT ImportContext
    (ExceptT ImportError
     (ReaderT Options IO))


-- | Perform VIN import action using the provided options.
runImport :: Import a -> Options -> Connection -> IO (Either ImportError a)
runImport act opts conn =
    flip runReaderT opts $ runExceptT $ do
      fid <- lift $ asks format
      vf <- liftIO $ Patch.read (Ident fid) conn
      case vf of
        Right vf' -> runReaderT act $ ImportContext conn vf'
        _         -> throwE UnknownVinFormat
