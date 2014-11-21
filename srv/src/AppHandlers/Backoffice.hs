{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppHandlers.Backoffice
    (
      -- * Back office operation
      openAction
    , dueCaseActions

      -- * Back office analysis
    , allActionResults
    , BORepr(..)
    , serveBackofficeSpec
    )

where

import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Attoparsec.Text
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time
import           Data.Typeable

import           GHC.TypeLits

import           Database.PostgreSQL.Simple  ((:.)(..), Only(..))
import           Snap

import           Carma.Model
import           Data.Model                  (F, PK, idents)
import qualified Data.Model.Sql              as Sql
import qualified Data.Model.Patch            as Patch
import qualified Data.Model.Patch.Sql        as Patch

import qualified Carma.Model.Action          as Action
import qualified Carma.Model.ActionResult    as ActionResult
import qualified Carma.Model.ActionType      as ActionType
import qualified Carma.Model.CaseStatus      as CaseStatus
import           Carma.Model.Event           (EventType(..))
import qualified Carma.Model.FalseCall       as FalseCall
import qualified Carma.Model.PaymentType     as PaymentType
import qualified Carma.Model.Program         as Program
import qualified Carma.Model.Role            as Role
import qualified Carma.Model.Satisfaction    as Satisfaction
import qualified Carma.Model.ServiceStatus   as ServiceStatus
import qualified Carma.Model.ServiceType     as ServiceType
import qualified Carma.Model.SmsTemplate     as SmsTemplate

import           Carma.Backoffice
import qualified Carma.Backoffice.DSL        as DSL
import           Carma.Backoffice.Graph
import           Carma.Backoffice.Text
import           Carma.Backoffice.Validation

import           AppHandlers.Util hiding (withPG)
import           Application
import           Snaplet.Auth.PGUsers
import           Util
import           Utils.Events


-- | Back office representation.
data BORepr = Txt
            | Dot
            -- ^
            | Check
            -- ^ Validation report.


-- | Translation tables to print constants in human-readable form.
type IdentMap m = Map.Map (IdentI m) Text


-- | Simple ident mapping
iMap :: Model m => IdentMap m
iMap = Map.fromList $ map (\(k, v) -> (v, T.pack k)) $ HM.toList idents


-- | Use labels set in database to pretty-print constants.
labelMap :: forall m n d d1. (KnownSymbol n, Model m) =>
            (m -> PK Int m d)
         -> (m -> F Text n d1)
         -> AppHandler (IdentMap m)
labelMap identField labelField = do
  let vals = HM.elems (idents :: HM.HashMap String (IdentI m))
  res <- withPG $
         \conn ->
           Sql.select
           (identField :. labelField :. (identField `Sql.sql_in` vals))
           conn
  return $ Map.fromList $
    map (\(Only aid :. Only label :. ()) -> (aid, label)) res


-- | Serve a pretty-printed back office processing report.
--
-- Ignore action results listed as comma-separated list of integer
-- codes in @skipResults@ request parameter.
serveBackofficeSpec :: BORepr -> AppHandler ()
serveBackofficeSpec repr = do
  -- Combine mappings for multiple models into one
  let maps = [ boxMap <$> labelMap ActionResult.ident ActionResult.label
             , boxMap <$> labelMap ActionType.ident ActionType.label
             , boxMap <$> labelMap CaseStatus.ident CaseStatus.label
             , boxMap <$> labelMap FalseCall.ident FalseCall.label
             , boxMap <$> labelMap PaymentType.ident PaymentType.label
             , boxMap <$> labelMap Role.ident Role.label
             , boxMap <$> labelMap Satisfaction.ident Satisfaction.label
             , boxMap <$> labelMap ServiceStatus.ident ServiceStatus.label
             , boxMap <$> labelMap ServiceType.ident ServiceType.label
             , boxMap <$> labelMap SmsTemplate.ident SmsTemplate.label
             , boxMap <$> labelMap Program.ident Program.label
             ]
  boxedIMap <- Map.unions <$> sequence maps
  skipParam <- liftM (parseOnly (decimal `sepBy1` (char ','))) <$>
               getParamT "skipResults"
  let skippedResults =
        case skipParam of
          Just (Right l) -> map Ident l
          Just (Left e) -> error e
          Nothing -> []
  (modifyResponse $ setContentType "text/plain; charset=UTF-8") >>
    case repr of
      Txt -> writeText $ backofficeText carmaBackoffice boxedIMap
      Dot -> writeLazyText $
             backofficeDot skippedResults carmaBackoffice boxedIMap
      Check -> writeJSON $ map show $ checkBackoffice carmaBackoffice boxedIMap
    where
      boxMap :: Model m => IdentMap m -> Map.Map IBox Text
      boxMap = Map.mapKeys IBox


-- | Serve JSON list of available results for all action types as list
-- of pairs @[ActionTypeI, ActionResultI]@.
--
-- @supervisorClosed@ results are stripped out if the user has no
-- supervisor role.
allActionResults :: AppHandler ()
allActionResults = do
  rls <- fromMaybe [] <$> currentUserRoles
  let isSupervisor = Role.supervisor `elem` rls
  writeJSON $
    filter (\(_, r) ->
              isSupervisor || r /= ActionResult.supervisorClosed) $
    concatMap (\a -> map (DSL.aType a,) $ DSL.actionResults a) $
    snd carmaBackoffice


data BackofficeError = NotYourAction
                       -- ^ A handler attempted to change an action
                       -- not assigned to the user.
                       deriving (Typeable, Show)


instance Exception BackofficeError


-- | Read @actionid@ request parameter and set @openTime@ of that
-- action to current time.
openAction :: AppHandler ()
openAction = do
  aid <- fromMaybe (error "Could not read actionid parameter") <$>
         getIntParam "actionid"
  uid <- currentUserMetaId
  now <- liftIO getCurrentTime
  let act = ExceptT (withPG (Patch.read aid'))
      aid' = Ident aid
      checkAuth a =
        unless (a `Patch.get'` Action.assignedTo == uid) $
        throwE $ SomeException NotYourAction
      p = Patch.put Action.openTime (Just now) Patch.empty
      upd = ExceptT (withPG (Patch.update aid' p))
  runExceptT (act >>= checkAuth >> upd) >>=
    \case
      Right r -> logCRUDState Update aid' p >>
                 writeJSON r
      Left  e -> error $ show e


-- | Read @caseid@ request parameter and serve JSON list of ids of
-- open actions in that case.
--
-- TODO Convert this to a read-trigger for Case.actions EF.
dueCaseActions :: AppHandler ()
dueCaseActions = do
  cid <- fromMaybe (error "Could not read caseid parameter") <$>
         getIntParam "caseid"
  res <- withPG $
         \conn ->
           Sql.select
           (Action.ident :.
            Action.caseId `Sql.eq` (Ident cid) :.
            Sql.isNull Action.result :.
            Sql.ascBy Action.ident :. Sql.descBy Action.closeTime)
           conn
  writeJSON $ map (\(Only aid :. ()) -> aid) res
