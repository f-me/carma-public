{-|

Type and monadic layout of VIN import process.

Transformer wrappers

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carma.VIN.Base
    ( ColumnTitle
    , Import
    , Options(..)
    , ImportContext(..)
    , ImportError(..)
    , runImport
    )

where

import Control.Applicative
import Data.Data
import Data.Functor

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Text (Text)
import Database.PostgreSQL.Simple

import Data.Model
import Data.Model.Patch     as Patch
import Data.Model.Patch.Sql as Patch

import Carma.Model.Program    (Program)
import Carma.Model.SubProgram (SubProgram)
import Carma.Model.Usermeta   (Usermeta)
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
    } deriving (Show, Data, Typeable)


data ImportContext = ImportContext
    { connection :: Connection
    , vinFormat  :: Patch VinFormat
    }


-- | Critical VIN import errors which result in the whole process
-- being interrupted.
data ImportError = ConnectionFailed
                 | NoTarget
                 | NoHeader
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
                  (\c -> close c)) $
                  \c -> do
                    vf <- liftIO $ Patch.read (Ident fid) c
                    case vf of
                      (vf':_) -> runReaderT act $ ImportContext c vf'
                      _       -> throwError UnknownVinFormat
