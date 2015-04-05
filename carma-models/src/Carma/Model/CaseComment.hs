{-# LANGUAGE TemplateHaskell #-}

module Carma.Model.CaseComment where

import Data.Text
import Data.Time.Clock
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Case.Type (Case)
import Carma.Model.Types()
import Carma.Model.PgTypes()
import Carma.Model.Usermeta (Usermeta)


data CaseComment = CaseComment
  { ident
    :: PK Int CaseComment "Комментарий к кейсу"
  , caseId
    :: F (IdentI Case) "caseId" "Кейс"
  , ctime
    :: F UTCTime "ctime" "Дата/время"
  , author
    :: F (IdentI Usermeta) "author" "Автор"
  , comment
    :: F Text "comment" "Комментарий"
  } deriving Typeable


instance Model CaseComment where
  type TableName CaseComment = "CaseComment"
  modelInfo = mkModelInfo CaseComment ident
  modelView = \case
    "" -> Just $ modifyView defaultView
          [ hiddenIdent caseId
          , readonly ctime
          , readonly author
          , readonly comment
          ]
    _  -> Nothing
