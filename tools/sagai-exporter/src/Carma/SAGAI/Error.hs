module Carma.SAGAI.Error
    ( ExportError(..)
    )

where

import Control.Monad.Trans.Error

import Carma.HTTP


data ExportError = NoField FieldName
                 | EmptyField FieldName
                 | UnexpectedFieldValue FieldName FieldValue
                 | UnknownProgram FieldValue
                 | UnknownService String
                 | UnknownTechType FieldValue
                 | BadTime FieldValue
                 | BadDays FieldValue
                   deriving Show

instance Error ExportError
