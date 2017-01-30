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
  { ident    :: PK Int DiagHistory ""
  , ctime    :: F UTCTime "ctime" "Дата создания"
  , userId   :: F (IdentI Usermeta) "userId" "Пользователь"
  , caseId   :: F (IdentI Case) "caseId" "Кейс"
  , slideId  :: F (IdentI DiagSlide) "slideId" "Слайд"
  , answerix :: F Int "answerIx" "Ответ"
  } deriving Typeable



instance Model DiagHistory where
  type TableName DiagHistory = "DiagHistory"
  modelInfo = mkModelInfo DiagHistory ident
  modelView = \case
    "" -> Just defaultView
    _  -> Nothing
