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


-- | A node in a directed graph of back office actions. Type parameter
-- selects a back office implementation (which depends on how the
-- graph is used).
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
    -- Time helpers (see also 'minutes', 'hours', 'days').
    now    :: impl UTCTime
    since  :: NominalDiffTime -> impl UTCTime -> impl UTCTime
    before :: NominalDiffTime -> impl UTCTime -> impl UTCTime
    before diff time = (-diff) `since` time

    -- Make an action assignable to users with the given role
    role :: IdentI Role -> impl ActionAssignment
    -- Keep an action assigned to the current user, but also make it
    -- available to a role
    currentUserOr :: IdentI Role -> impl ActionAssignment

    -- Source action which led to this one
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

    -- Lift idents for use with comparison combinators
    const :: Model v =>
             IdentI v -> impl (IdentI v)

    -- List membership predicate
    oneOf :: Model v =>
             impl (IdentI v) -> [IdentI v] -> impl Bool

    -- Branching
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

    closeWith :: [ActionTypeI] -> ActionResultI -> impl ()

    -- Control flow combinators
    finish  :: impl ActionOutcome
    proceed :: [ActionTypeI] -> impl ActionOutcome
    defer   :: impl ActionOutcome

    -- Action chains
    infixr *>
    (*>) :: impl () -> impl ActionOutcome -> impl ActionOutcome


setServiceStatus :: Backoffice impl => IdentI ServiceStatus -> impl ()
setServiceStatus = setServiceField Service.status


minutes :: Num i => i
minutes = 60


hours :: Num i => i
hours = 60 * minutes


days :: Num i => i
days = 24 * hours


data Entry =
    Entry { trigger :: forall impl. (Backoffice impl) => impl Trigger
          , result  :: forall impl. (Backoffice impl) => impl ActionOutcome
          }


-- | Formal description of how a back office operates.
type BackofficeSpec = ([Entry], [Action])
