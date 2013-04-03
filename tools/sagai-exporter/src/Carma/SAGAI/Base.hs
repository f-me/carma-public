{-|

Types and monadic layout of data used when exporting a case with
services to SAGAI format.

-}

module Carma.SAGAI.Base
    ( ExportData
    , Service
    , ExportState(..)
    , ExportDicts(..)
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

import Data.ByteString as BS
import Data.Dict as D
import Data.Text.ICU.Convert

import Carma.HTTP

import Carma.SAGAI.Codes


-- | A case instance and a list of services attached to this case.
type ExportData = (InstanceData, [Service])


-- | Model name, id and data of a service.
type Service = (String, Int, InstanceData)


data ExportState = ExportState { counter :: Int
                               -- ^ Line counter used for @SEP@ field.
                               , content :: ByteString
                               -- ^ All SAGAI entry content generated
                               -- so far.
                               }


-- | Various dictionaries used during export process to map values
-- stored in fields to corresponding dictionary labels.
data ExportDicts = ExportDicts { wazzup :: D.Dict
                               -- ^ Dictionary used on the @comment@
                               -- field of a case.
                               , techTypes :: D.Dict
                               -- ^ Dictionary used on the @techType@
                               -- field of a @tech@ service.
                               , carClasses :: D.Dict
                               -- ^ Dictionary used on the @carClass@
                               -- field of a @rent@ service.
                               }


-- | Read only options used when processing a case.
data ExportOptions = ExportOptions { carmaPort :: Int
                                   -- ^ CaRMa port.
                                   , dicts :: ExportDicts
                                   , utfConv :: Converter
                                   -- ^ A converter used to encode
                                   -- text from UTF-8 to target
                                   -- encoding.
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
               | UnknownProgram FieldValue
               | UnknownService String
               | UnknownTechType FieldValue
               | UnreadableContractorId FieldValue
               | UnknownDictValue FieldValue
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
          -> ExportDicts
          -> String
          -- ^ Name of an output character set.
          -> IO (Either ExportError ((a, ExportState), [String]))
runExport act sepStart input cp ds eName = do
    e <- open eName Nothing
    let options = ExportOptions cp ds e
        inner = runReaderT (runStateT act $ ExportState sepStart BS.empty) $
                (input, options)
    res <- runErrorT $ runWriterT inner
    return res
