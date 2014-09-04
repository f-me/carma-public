{-# LANGUAGE DeriveDataTypeable #-}

module AppHandlers.Backoffice
    (
      openAction

      -- * Back office analysis
    , BORepr(..)
    , serveBackofficeSpec
    )

where

import           Control.Exception
import           Control.Monad.Trans.Except
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Typeable

import           Snap

import           Carma.Model
import           Data.Model                  (idents)
import qualified Data.Model.Patch            as Patch
import qualified Data.Model.Patch.Sql        as Patch

import qualified Carma.Model.Action          as Action
import           Carma.Model.ActionResult    (ActionResult)
import           Carma.Model.ActionType      (ActionType)
import           Carma.Model.CaseStatus      (CaseStatus)
import           Carma.Model.FalseCall       (FalseCall)
import           Carma.Model.Program         (Program)
import qualified Carma.Model.Role            as Role
import           Carma.Model.Satisfaction    (Satisfaction)
import           Carma.Model.ServiceStatus   (ServiceStatus)
import           Carma.Model.ServiceType     (ServiceType)
import           Carma.Model.SmsTemplate     (SmsTemplate)

import           Carma.Backoffice
import           Carma.Backoffice.Graph
import           Carma.Backoffice.Text
import           Carma.Backoffice.Validation

import           AppHandlers.Util hiding (withPG)
import           Application
import           Snaplet.Auth.PGUsers
import           Util


data BORepr = Txt | Dot | Check


type IdentMap m = Map.Map (IdentI m) Text


serveBackofficeSpec :: BORepr -> AppHandler ()
serveBackofficeSpec repr =
    case repr of
      Txt -> writeText $ backofficeText carmaBackoffice boxedIMap
      Dot -> writeLazyText $ backofficeDot carmaBackoffice boxedIMap
      Check -> writeJSON $ map show $ checkBackoffice carmaBackoffice boxedIMap
    where
      -- Simple ident mapping
      iMap :: Model m => IdentMap m
      iMap = Map.fromList $ map (\(k, v) -> (v, T.pack k)) $ HM.toList idents

      boxMap :: Model m => IdentMap m -> Map.Map IBox Text
      boxMap = Map.mapKeys IBox
      -- Combine mappings for multiple models into one
      boxedIMap = Map.unions [ boxMap (iMap :: IdentMap ActionResult)
                             , boxMap (iMap :: IdentMap ActionType)
                             , boxMap (iMap :: IdentMap CaseStatus)
                             , boxMap (iMap :: IdentMap FalseCall)
                             , boxMap (iMap :: IdentMap Role.Role)
                             , boxMap (iMap :: IdentMap Satisfaction)
                             , boxMap (iMap :: IdentMap ServiceStatus)
                             , boxMap (iMap :: IdentMap ServiceType)
                             , boxMap (iMap :: IdentMap SmsTemplate)
                             , boxMap (iMap :: IdentMap Program)
                             ]

--actionResults :: AppHandler (


data BackofficeError = NotYourAction
                       deriving (Typeable, Show)

instance Exception BackofficeError


-- | Read @actionid@ request parameter and set @openTime@ of that
-- action to current time.
openAction :: AppHandler ()
openAction = do
  aid <- getIntParam "actionid"
  case aid of
    Nothing -> error "Could not read actionid parameter"
    Just i -> do
      uid <- currentUserMetaId
      now <- liftIO $ getCurrentTime
      let act = ExceptT (withPG (Patch.read (Ident i)))
          checkAuth a = if a `Patch.get'` Action.assignedTo == uid
                        then return ()
                        else throwE $ SomeException NotYourAction
          p = Patch.put Action.openTime (Just now) $ Patch.empty
          upd = ExceptT (withPG (Patch.update (Ident i) p))
      runExceptT (act >>= checkAuth >> upd) >>=
        \case
          Right r -> writeJSON r
          Left  e -> error $ show e
