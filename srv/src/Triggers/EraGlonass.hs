{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, MultiWayIf, LambdaCase #-}

module Triggers.EraGlonass
     ( handleEraGlonassCaseStatusByService
     ) where

import           Prelude hiding (fail)

import           Data.Proxy
import           Data.Typeable (typeRep)
import           Data.Maybe (listToMaybe)
import           Data.Function ((&))
import           Data.String (fromString)
import           Text.InterpolatedString.QM

import           Control.Monad ((>=>), when, void)
import           Control.Monad.Free (Free)

import           Data.Model.Patch (get')
import qualified Data.Model.Patch as Patch
import           Data.Model.Types (IdentI)
import           Data.Model.Utils.PostgreSQL.MSqlQQ hiding (parseQuery)

import           Snap.Snaplet.PostgresqlSimple (query)

import           Triggers.DSL (Dsl)
import qualified Triggers.DSL as Dsl

import           Carma.Model.Case (Case)
import qualified Carma.Model.Case as Case
import           Carma.Model.Service (Service)
import qualified Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.EraGlonassCaseStatusUpdate
                            (EraGlonassCaseStatusUpdate)
import qualified Carma.Model.EraGlonassCaseStatusUpdate as EGCaseStatusUpdate


type Services = [(IdentI Service, IdentI ServiceStatus)]

handleEraGlonassCaseStatusByService :: IdentI Service -> Free (Dsl m) ()
handleEraGlonassCaseStatusByService = guarded where
  guarded serviceId = do
    service <- Dsl.dbRead serviceId
    case'   <- Dsl.dbRead $ service `get'` Service.parentId
    when (case' `get'` Case.isCreatedByEraGlonass) (go case' service)

  resolve :: Services -> Maybe NewStatus
  resolve services = go' where
    x = snd <$> services
    closed = [SS.mistake, SS.ok, SS.canceled]

    go' | any (`notElem` closed)                 x = Just WorkInProgress
        | all (`elem` [SS.ok,       SS.mistake]) x = Just Done
        | all (`elem` [SS.canceled, SS.mistake]) x = Just ClientDenial
        | all (`elem` closed)                    x = Just Done
        | otherwise                                = Nothing

  go case' service = do
    let caseId = case' `get'` Case.ident

    (caseServices :: Services) <-
      Dsl.doApp $ uncurry query [msql|
        SELECT $(F|Service.ident)$, $(F|Service.status)$
        FROM $(T|Service)$
        WHERE $(F|Service.parentId)$ = $(V|caseId)$
      |]

    let resolved =
          case caseServices of
               xs@(_ : _) -> resolve xs

               [] -> error [qms|
                  List of "{typeRep (Proxy :: Proxy Service)}"s for
                  "{typeRep (Proxy :: Proxy Case)}" #{case' `get'` Case.ident}
                  is unexpectedly empty! Triggered by
                  "{typeRep (Proxy :: Proxy Service)}"
                  #{service `get'` Service.ident}!
               |]

    let findLastDuplicate
          :: Free (Dsl m) (Maybe (IdentI EraGlonassCaseStatusUpdate))

        findLastDuplicate
          = fmap (listToMaybe >=> listToMaybe)
          $ Dsl.doApp
          $ uncurry query [msql|
            SELECT $(F|EGCaseStatusUpdate.ident)$
            FROM $(T|EGCaseStatusUpdate)$

            WHERE $(F|EGCaseStatusUpdate.isProcessed)$ = $(V|False)$

              AND $(F|EGCaseStatusUpdate.caseId)$ =
                  $(V|case' `get'` Case.ident)$

            ORDER BY $(F|EGCaseStatusUpdate.ctime)$ DESC
            LIMIT 1
          |]

    now <- Dsl.getNow

    case resolved of
         Just newCaseStatus@WorkInProgress ->
           findLastDuplicate >>= \case
             Just prevId ->
               void $ Dsl.dbUpdate prevId $ Patch.empty
                 & Patch.put EGCaseStatusUpdate.mtime (Just now)

                 & Patch.put EGCaseStatusUpdate.newCaseStatus
                             (fromString $ show newCaseStatus)

             Nothing ->
               void $ Dsl.dbCreate $ Patch.empty
                 & Patch.put EGCaseStatusUpdate.ctime now
                 & Patch.put EGCaseStatusUpdate.caseId (case' `get'` Case.ident)

                 & Patch.put EGCaseStatusUpdate.newCaseStatus
                             (fromString $ show newCaseStatus)

                 & Patch.put EGCaseStatusUpdate.isProcessed False

         Just newCaseStatus ->
           findLastDuplicate >>= \case
             Just prevId ->
               void $ Dsl.dbUpdate prevId $ Patch.empty
                 & Patch.put EGCaseStatusUpdate.mtime (Just now)

                 & Patch.put EGCaseStatusUpdate.newCaseStatus
                             (fromString $ show newCaseStatus)

                 & Patch.put EGCaseStatusUpdate.customerName
                             (case' `get'` Case.contact_name)

                 & Patch.put EGCaseStatusUpdate.customerPhone
                             (case' `get'` Case.contact_phone1)

                 & Patch.put EGCaseStatusUpdate.terminalPhone
                             (case' `get'` Case.contact_phone2)

             Nothing ->
               void $ Dsl.dbCreate $ Patch.empty
                 & Patch.put EGCaseStatusUpdate.ctime now
                 & Patch.put EGCaseStatusUpdate.caseId (case' `get'` Case.ident)

                 & Patch.put EGCaseStatusUpdate.newCaseStatus
                             (fromString $ show newCaseStatus)

                 & Patch.put EGCaseStatusUpdate.isProcessed False

                 & Patch.put EGCaseStatusUpdate.customerName
                             (case' `get'` Case.contact_name)

                 & Patch.put EGCaseStatusUpdate.customerPhone
                             (case' `get'` Case.contact_phone1)

                 & Patch.put EGCaseStatusUpdate.terminalPhone
                             (case' `get'` Case.contact_phone2)

         Nothing -> pure ()


data NewStatus = WorkInProgress | Done | ClientDenial deriving Eq

instance Show NewStatus where
  show = \case
    WorkInProgress -> "WORK_IN_PROGRESS"
    Done           -> "DONE"
    ClientDenial   -> "CLIENT_DENIAL"
