{-|

Types and monadic layout of data used when exporting a case with
services to SAGAI format.

-}

module Carma.SAGAI.Base
    ( ExportData
    , Service
    , ExportState(..)
    , ExportOptions(..)
    -- * Export monads
    , CaseExport
    , ServiceExport
    , runExport
    -- * Errors
    , ExportError(..)
    , ErrorType(..)
    )

where

import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Dict as D

import Carma.HTTP

import Carma.SAGAI.Codes


-- | A case instance and a list of services attached to this case.
type ExportData = (InstanceData, [Service])


-- | Model name, id and data of a service.
type Service = (String, Int, InstanceData)


data ExportState = ExportState { counter :: Int
                               -- ^ Line counter used for @SEP@ field.
                               }


-- | Read only options used when processing a case.
data ExportOptions = ExportOptions { carmaPort :: Int
                                   -- ^ CaRMa port.
                                   , wazzup :: D.Dict
                                   -- ^ Dictionary used on the @comment@
                                   -- field of a case.
                                   }


-- | Main monad used to form a SAGAI entry for a case. Reader state
-- stores the case and its services. Writer state keeps log messages.
-- Error monad is provided to early terminate entry export in case of
-- critical errors. IO may be used to query CaRMa database.
type CaseExport =
    (StateT ExportState
     (ReaderT (ExportData, ExportOptions)
      (WriterT [String]
       (ErrorT ExportError IO))))


-- | A sub-monad used when forming a part of a SAGAI entry
-- corresponding to a service. Provides easy access to the currently
-- processed service and its expense type.
type ServiceExport =
    ReaderT (Service, ExpenseType) CaseExport


-- | Critical error during SAGAI case export process.
data ExportError = CaseError ErrorType
                 | ServiceError String Int ErrorType
                 -- ^ Error occured when processing one of services
                 -- attached to a case. Model name and id of service
                 -- are stored.
                   deriving Show


data ErrorType = NoField FieldName
               | EmptyField FieldName
               | UnexpectedFieldValue FieldName FieldValue
               | UnknownProgram FieldValue
               | UnknownService String
               | UnknownTechType FieldValue
               | UnreadableContractorId FieldValue
               | BadTime FieldValue
               | BadDays FieldValue
               | BadVin FieldValue
                 deriving Show


instance Error ExportError


-- | Perform export action using the provided case and services data
-- and export options. If no errors occured, then return action
-- result, final state of export monad and log of operations for this
-- case.
runExport :: CaseExport a
          -> Int
          -- ^ Initial value for @SEP@ line counter.
          -> ExportData
          -- ^ Case and all of its services.
          -> Int
          -- ^ CaRMa port.
          -> D.Dict
          -- ^ Wazzup dictionary.
          -> IO (Either ExportError ((a, ExportState), [String]))
runExport act sepStart input cp wz = do
    let inner = runReaderT (runStateT act $ ExportState sepStart) $
                (input, ExportOptions cp wz)
    res <- runErrorT $ runWriterT inner
    return res
