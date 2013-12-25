{-|

Type and monadic layout of VIN import process.

Transformer wrappers

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Carma.VIN.Base
    ( Import
    , Options(..)
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

import Database.PostgreSQL.Simple

import Data.Model

import Carma.Model.Program    (Program)
import Carma.Model.SubProgram (SubProgram)
import Carma.Model.Usermeta   (Usermeta)
import Carma.Model.VinFormat  (VinFormat)


deriving instance Data ConnectInfo


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
    }


-- | Critical VIN import errors which result in the whole process
-- being interrupted.
data ImportError = IE String
                 | ConnectionFailed
                 | NoTarget
                   deriving Show


instance Error ImportError


-- | Base monad.
type Import = ReaderT ImportContext (ErrorT ImportError (ReaderT Options IO))


-- | Perform VIN import action using the provided options.
runImport :: Import a -> Options -> IO (Either ImportError a)
runImport act opts =
    flip runReaderT opts $ runErrorT $ do
      -- Check if we will possibly be able to obtain a subprogram to
      -- store contracts in.
      (,) <$> (lift $ asks program) <*> (lift $ asks subprogram) >>=
              \case
              (Nothing, Nothing) -> throwError NoTarget
              -- Close connection when short-circuiting Import monad
              _                  -> liftBaseOp
                                    (bracket
                                     (print "conn" >> (connect $ cInfo opts))
                                     (\c -> close c >> print "Closed"))
                                    (\c -> runReaderT act $ ImportContext c)
