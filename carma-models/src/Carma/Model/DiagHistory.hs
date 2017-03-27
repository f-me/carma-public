module Carma.Model.DiagHistory where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable

import Data.Model
import Data.Model.View

import Carma.Model.Case      (Case)
import Carma.Model.Usermeta  (Usermeta)
import Carma.Model.DiagSlide (DiagSlide)

import Carma.Model.Types()
import Carma.Model.PgTypes()


data DiagHistory = DiagHistory
  { ident        :: PK Int DiagHistory ""
  , ctime        :: F UTCTime "ctime" "Дата создания"
  , caseId       :: F (IdentI Case) "caseId" "Кейс"
  , slideId      :: F (IdentI DiagSlide) "slideId" "Слайд"
  , createdBy    :: F (IdentI Usermeta) "createdBy" "Кто создал вопрос"
  , answerIx     :: F Int "answerIx" "Ответ"
  , answeredBy   :: F (Maybe (IdentI Usermeta)) "answeredBy" "Кто ответил"
  , answerTime   :: F (Maybe UTCTime) "answerTime" "Дата ответа"
  , deprecatedBy :: F (Maybe (IdentI DiagHistory)) "deprecatedBy" "Ответ отменён"
  } deriving Typeable


instance Model DiagHistory where
  type TableName DiagHistory = "DiagHistory"
  modelInfo = mkModelInfo DiagHistory ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
