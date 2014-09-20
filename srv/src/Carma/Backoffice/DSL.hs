{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

{-|

All you need to define a back office logic: high-level types and DSL
terms.

-}

module Carma.Backoffice.DSL
    (
      -- * Back office types
      ActionTypeI
    , ActionResultI
    , Action(..)
    , Entry(..)
    , BackofficeSpec

     -- * Back office language
    , Outcome
    , Trigger
    , Backoffice(..)
    , Scope(..)
    , MailType(..)
    , finish
    , setServiceStatus
    , ite
    , minutes
    , hours
    , days

    -- * Misc
    , actionResults
    )

where

import           Prelude hiding (const)

import           Data.Time.Clock

import           Data.Model
import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import           Carma.Model.ActionType (ActionType)
import qualified Carma.Model.Action as CarmaAction
import           Carma.Model.Case as Case
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import           Carma.Model.SmsTemplate (SmsTemplate)
import           Carma.Model.Usermeta (Usermeta)

import           Carma.Backoffice.DSL.Types


type ActionTypeI = IdentI ActionType


type ActionResultI = IdentI ActionResult


-- | A node in a directed graph of back office actions. Various action
-- aspects are described using 'Backoffice' DSL terms.
data Action =
    Action { aType      :: ActionTypeI
           -- ^ Binds action definition to its name and
           -- description as stored in the database.
           , targetRole :: forall impl. (Backoffice impl) =>
                           impl (IdentI Role)
           , assignment :: forall impl. (Backoffice impl) =>
                           impl (Maybe (IdentI Usermeta))
           -- ^ Default target role/user for newly created actions of
           -- this type.
           , due        :: forall impl. (Backoffice impl) =>
                           impl UTCTime
           -- ^ Default due time for new actions.
           , outcomes   :: forall impl. (Backoffice impl) =>
                           [(ActionResultI, impl (Outcome CarmaAction.Action))]
           -- ^ All action results (outward node edges).
           }


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

    -- | No user.
    nobody :: impl (Maybe (IdentI Usermeta))

    currentUser :: impl (Maybe (IdentI Usermeta))

    -- | Assignee of the last matching action.
    assigneeOfLast :: Scope
                      -- ^ Where to look for actions.
                   -> [ActionTypeI]
                   -- ^ Matching action types.
                   -> [impl (Maybe ActionResultI)]
                   -- ^ A result used to close the matched actions.
                   -> impl (Maybe (IdentI Usermeta))

    -- | Empty result of open actions.
    noResult :: impl (Maybe ActionResultI)

    -- | Source action which led to this one. If there was no previous
    -- action (e.g. when the action was created from an 'Entry'), this
    -- is guaranteed to return an action type not equal to any other
    -- used action types.
    previousAction :: impl ActionTypeI

    -- | Service context access.
    --
    -- Our DSL is stateless (more accurately, there's a read-only
    -- state accessed by these getters; it's not tied with mutators
    -- like 'setServiceField'). Data available through these getters
    -- must be thought of as «old» (in terms of Postgres triggers).
    --
    -- Note that @t@ type is not brought into DSL type system, thus an
    -- extra hint is included in type constraints for the
    -- meta-language interpreter.
    --
    -- Context access functions are not total: if there's no
    -- corresponding context during the run time, an error is raised.
    -- For instance, 'serviceField's cannot be accessed in case
    -- triggers or handlers for actions which are not attached to any
    -- service. Fixing the latter problem would involve tying
    -- ActionType and availability of a service for it (on the type
    -- level).
    serviceField  :: (FieldI t n d, HaskellType t ~ t) =>
                     (Service -> F t n d) -> impl t

    -- | Case access (always available).
    caseField     :: (FieldI t n d, HaskellType t ~ t) =>
                     (Case -> F t n d) -> impl t

    -- | Current user access (always available).
    userField     :: (FieldI t n d, HaskellType t ~ t) =>
                     (Usermeta -> F t n d) -> impl t

    -- | Trigger constructor.
    --
    -- It's implemented as a single term because we need to bind
    -- together a model @m@ in the field accessor and the outcome.
    -- 'PreContextAccess' constraint indicates the ability to
    -- bootstrap a context required for the meta-language interpreter.
    onField :: (PreContextAccess m, Model m,
                Eq t, FieldI t n d, HaskellType t ~ t) =>
               (m -> F t n d)
            -> impl t
            -> impl (Outcome m)
            -> impl Trigger

    -- | Like 'onField', but prevents actual changes to the triggered
    -- field.
    insteadOf :: (PreContextAccess m, Model m,
                  Eq t, FieldI t n d, HaskellType t ~ t) =>
                 (m -> F t n d)
              -> impl t
              -> impl (Outcome m)
              -> impl Trigger

    -- | Negation.
    --
    -- Boolean combinators are lifted to impl because we usually use
    -- terms from impl as arguments).
    not  :: impl Bool -> impl Bool

    -- | Equality.
    --
    -- Another example of meta-language type hints included in DSL
    -- types. Our type system is fully transparent here: for instance,
    -- we can compare two service fields with types completely unknown
    -- to our DSL while still being able to interpret them in text or
    -- in Haskell.
    infix 4 ==
    (==) :: Eq (HaskellType v) =>
            impl v -> impl v -> impl Bool
    -- | Greater-than.
    infix 4 >
    (>)  :: Ord (HaskellType v) =>
            impl v -> impl v -> impl Bool
    -- | Boolean AND.
    infixr 3 &&
    (&&) :: impl Bool -> impl Bool -> impl Bool
    -- | Boolean OR.
    infixr 2 ||
    (||) :: impl Bool -> impl Bool -> impl Bool

    -- | Lift idents for use with comparison combinators.
    const :: Model v =>
             IdentI v -> impl (IdentI v)

    -- | 'const' for optional values.
    just :: Model v =>
            IdentI v -> impl (Maybe (IdentI v))

    -- | Require a value.
    req :: impl (Maybe v) -> impl v

    -- | List membership predicate.
    oneOf :: Model v =>
             impl (IdentI v) -> [IdentI v] -> impl Bool

    -- | Branching.
    switch :: [(impl Bool, impl v)]
           -- ^ List of condition/value pairs. The first condition to
           -- be true selects the value of the expression.
           -> impl v
           -- ^ Default branch (used when no true conditions occured).
           -> impl v

    -- | Change a field in the service (if there's one).
    --
    -- See also docs for 'serviceField'.
    setServiceField :: (SvcAccess m, FieldI t n d, HaskellType t ~ t) =>
                       (Service -> F t n d) -> impl t -> impl (Eff m)

    sendMail :: MailType -> impl (Eff m)

    sendSMS  :: IdentI SmsTemplate -> impl (Eff m)

    -- | Close due actions of matching type.
    closePrevious :: Scope
                  -- ^ Where to look for actions.
                  -> [ActionTypeI]
                  -- ^ Matching action types.
                  -> ActionResultI
                  -- ^ A result used to close the matched actions.
                  -> impl (Eff m)

    -- | Create new actions of given types.
    --
    -- Duplicate types are ignored. When the list is is empty, no
    -- further processing is performed ('finish').
    proceed :: PreContextAccess m => [ActionTypeI] -> impl (Outcome m)

    -- | Postpone the action.
    defer   :: impl (Outcome CarmaAction.Action)

    -- | Action chains.
    --
    -- A control flow combinator always ends the
    -- chain:
    --
    -- > sendSMS *> doStuff *> finish
    infixr *>
    (*>) :: impl (Eff m) -> impl (Outcome m) -> impl (Outcome m)


-- | Do nothing.
finish :: (Backoffice impl, PreContextAccess m) => impl (Outcome m)
finish = proceed []


setServiceStatus :: (SvcAccess m, Backoffice impl) =>
                    IdentI ServiceStatus -> impl (Eff m)
setServiceStatus s = setServiceField Service.status (const s)


-- | If-then-else.
ite :: Backoffice impl =>
       impl Bool
    -- ^ If condition.
    -> impl v
    -- ^ Then branch.
    -> impl v
    -- ^ Else branch.
    -> impl v
ite cond t = switch [(cond, t)]


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
          }


-- | Formal description of how a back office operates (entries and all
-- graph nodes; indirect addressing is used).
--
-- Due to the indirect addressing it's easy to define an incomplete
-- graph, so be sure to validate it using 'checkBackoffice' prior to
-- use.
type BackofficeSpec = ([Entry], [Action])


-- | Extract a list of possible results from an action.
--
-- We cannot apply 'outcomes' without interpreting the result (@impl@
-- type variable would be ambiguous). Perhaps this may be implemented
-- differently if we make the type of 'outcomes' impredicative,
-- burying Backoffice class constraint deeper in the tuple. Wrapping
-- the right part of the tuple in an existential would require more
-- typing (pun intended).
actionResults :: Action -> [ActionResultI]
actionResults (Action _ _ _ _ outs) =
  map fst (outs :: [(ActionResultI, Any (Outcome CarmaAction.Action))])


data Any k


instance Backoffice Any
