{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Carma.Backoffice.DSL
    (
      -- * Back office types
      ActionTypeI
    , ActionResultI
    , Action(..)
    , Entry(..)
    , BackofficeSpec

     -- * Back office language
    , ActionAssignment
    , ActionOutcome
    , Trigger
    , Backoffice(..)
    , setServiceStatus
    , minutes
    , hours
    , days
    )

where

import           Data.Time.Clock

import           Data.Model
import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import           Carma.Model.ActionType (ActionType)
import           Carma.Model.Case.Type as Case
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import           Carma.Model.SmsTemplate (SmsTemplate)
import           Carma.Model.Usermeta (Usermeta)


type ActionTypeI = IdentI ActionType


type ActionResultI = IdentI ActionResult


-- | A node in a directed graph of back office actions. Various action
-- aspects are described using 'Backoffice' DSL terms.
data Action =
    Action { aType      :: ActionTypeI
           -- ^ Binds action definition to its name and
           -- description as stored in the database.
           , assignment :: forall impl. (Backoffice impl) =>
                           impl ActionAssignment
           -- ^ Default target role/user for newly created actions of
           -- this type.
           , due        :: forall impl. (Backoffice impl) =>
                           impl UTCTime
           -- ^ Default due time for new actions.
           , outcomes   :: forall impl. (Backoffice impl) =>
                           [(ActionResultI, impl ActionOutcome)]
           -- ^ All action results (outward node edges).
           }


data ActionAssignment


data ActionOutcome


data Trigger


-- | Back office language (typed tagless final representation).
class Backoffice impl where
    -- | Current time.
    now    :: impl UTCTime

    since  :: NominalDiffTime
           -- ^ This long ..
           -> impl UTCTime
           -- ^ .. from this time.
           -> impl UTCTime

    before :: NominalDiffTime
           -- ^ This long ..
           -> impl UTCTime
           -- ^ .. before this time.
           -> impl UTCTime
    before diff time = (-diff) `since` time

    -- | Make an action assignable to users with the given role.
    role :: IdentI Role -> impl ActionAssignment

    -- | Keep an action assigned to the current user, but also make it
    -- available to the role.
    currentUserOr :: IdentI Role -> impl ActionAssignment

    -- | Source action which led to this one. If there was no previous
    -- action (e.g. when the action was created from an 'Entry'), this
    -- is guaranteed to return an action type not equal to any other
    -- used action types.
    previousAction :: impl ActionTypeI

    -- Context access
    userField     :: FieldI t n d =>
                     (Usermeta -> F t n d) -> impl t
    caseField     :: FieldI t n d =>
                     (Case -> F t n d) -> impl t
    serviceField  :: FieldI t n d =>
                     (Service -> F t n d) -> impl t
    serviceField' :: FieldI t n d =>
                     (Service -> F (Maybe t) n d) -> impl t

    onCaseField :: FieldI t n d =>
                   (Case -> F t n d)
                -> impl t
                -> impl Trigger
    onServiceField :: FieldI t n d =>
                      (Service -> F t n d)
                   -> impl t
                   -> impl Trigger
    onServiceField' :: FieldI t n d =>
                       (Service -> F (Maybe t) n d)
                    -> impl t
                    -> impl Trigger


    -- Boolean combinators (lifted to impl because we usually use
    -- terms from impl as arguments)
    not  :: impl Bool -> impl Bool
    infix 4 >
    (>)  :: Ord v => impl v -> impl v -> impl Bool
    infix 4 ==
    (==) :: Eq v => impl v -> impl v -> impl Bool
    infixr 3 &&
    (&&) :: impl Bool -> impl Bool -> impl Bool
    infixr 2 ||
    (||) :: impl Bool -> impl Bool -> impl Bool

    -- | Lift idents for use with comparison combinators.
    const :: Model v =>
             IdentI v -> impl (IdentI v)

    -- | List membership predicate.
    oneOf :: Model v =>
             impl (IdentI v) -> [IdentI v] -> impl Bool

    -- | Branching operator.
    switch :: [(impl Bool, impl v)]
           -- ^ List of condition/value pair. The first condition to
           -- be true selects the value of the expression.
           -> impl v
           -- ^ Default branch (used when no true conditions occured).
           -> impl v

    -- Verbs with side effects
    setServiceField :: (Model m, t ~ (IdentI m), FieldI t n d) =>
                       (Service -> F t n d) -> t -> impl ()
    sendDealerMail :: impl ()
    sendGenserMail :: impl ()
    sendPSAMail    :: impl ()
    sendSMS        :: IdentI SmsTemplate -> impl ()

    -- | Close all previously created actions in the case.
    closeWith :: [ActionTypeI]
              -- ^ Matching action types.
              -> ActionResultI
              -- ^ A result used to close actions.
              -> impl ()

    -- | Close the action.
    finish  :: impl ActionOutcome

    -- | Close the action and create new actions of given types.
    proceed :: [ActionTypeI] -> impl ActionOutcome

    -- | Postpone the action.
    defer   :: impl ActionOutcome

    -- | Action chains.
    --
    -- A control flow combinator always ends the
    -- chain:
    --
    -- > sendSMS *> doStuff *> finish
    infixr *>
    (*>) :: impl () -> impl ActionOutcome -> impl ActionOutcome


setServiceStatus :: Backoffice impl => IdentI ServiceStatus -> impl ()
setServiceStatus = setServiceField Service.status


-- | 60 seconds
--
-- Matches Num instance of 'NominalDiffTime', so that
--
-- > 2 * minutes :: NominalDiffTime
--
-- means two minutes of time difference.
minutes :: Num i => i
minutes = 60


-- | 60 'minutes'.
hours :: Num i => i
hours = 60 * minutes


-- | 24 'hours'.
days :: Num i => i
days = 24 * hours


-- | An entry point in an action graph.
data Entry =
    Entry { trigger :: forall impl. (Backoffice impl) => impl Trigger
          , result  :: forall impl. (Backoffice impl) => impl ActionOutcome
          }


-- | Formal description of how a back office operates (entries and all
-- graph nodes; indirect addressing is used).
--
-- Due to the indirect addressing it's easy to define an incomplete
-- graph, so be sure to validate it using 'checkBackoffice' prior to
-- use.
type BackofficeSpec = ([Entry], [Action])
