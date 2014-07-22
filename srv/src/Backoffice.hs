{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Backoffice ()

where

import           Prelude hiding ((>), (==), (||), (&&), const)
import qualified Prelude ((>), (==), (||), (&&))

import           Control.Applicative
import           Data.Text as T hiding (concatMap, filter, map, zip)

import           Data.Time.Clock

import           Data.Model
import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import qualified Carma.Model.ActionResult as ActionResult
import           Carma.Model.ActionType (ActionType)
import qualified Carma.Model.ActionType as ActionType
import           Carma.Model.Case.Type as Case
import           Carma.Model.Program as Program
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.ServiceType as ServiceType
import           Carma.Model.SmsTemplate (SmsTemplate)
import qualified Carma.Model.SmsTemplate as SMS


-- | A node in a directed graph of back office actions. Type parameter
-- selects a back office implementation (which depends on how the
-- graph is used).
data Action impl =
    Action { aType      :: IdentI ActionType
           -- ^ Binds action definition to its name and
           -- description as stored in the database.
           , targetRole :: IdentI Role
           -- ^ Default target role for newly created actions
           -- of this type.
           , due        :: impl Pure UTCTime
           -- ^ Default due time for new actions.
           , outcomes   :: [(IdentI ActionResult, impl Eff ActionOutcome)]
           -- ^ All action results (outward node edges).
           }


-- | Forces us to use one of the action-ending verbs from Backoffice
-- DSL.
data ActionOutcome


-- | Type tag for effect control. Pure and Eff are lifted to type
-- level.
data Effects = Pure | Eff


-- | Back office language (typed tagless final representation).
class (Applicative (impl Eff)) => Backoffice impl where
    -- Time helpers (see also 'minutes', 'hours', 'days').
    now    :: impl Pure UTCTime
    since  :: NominalDiffTime -> impl Pure UTCTime -> impl Pure UTCTime
    before :: NominalDiffTime -> impl Pure UTCTime -> impl Pure UTCTime
    before diff time = (-diff) `since` time

    -- Context access
    caseField     :: FieldI t n d =>
                     (Case -> F t n d) -> impl Pure t
    caseField'    :: FieldI t n d =>
                     (Case -> F (Maybe t) n d) -> impl Pure t
    serviceField  :: FieldI t n d =>
                     (Service -> F t n d) -> impl Pure t
    serviceField' :: FieldI t n d =>
                     (Service -> F (Maybe t) n d) -> impl Pure t

    -- Boolean combinators (lifted to impl because we usually use
    -- terms from impl as arguments)
    not  :: impl Pure Bool -> impl Pure Bool
    infix 4 >
    (>)  :: Ord v => impl Pure v -> impl Pure v -> impl Pure Bool
    infix 4 ==
    (==) :: Eq v => impl Pure v -> impl Pure v -> impl Pure Bool
    infixr 3 &&
    (&&) :: impl Pure Bool -> impl Pure Bool -> impl Pure Bool
    infixr 2 ||
    (||) :: impl Pure Bool -> impl Pure Bool -> impl Pure Bool

    const :: v -> impl Pure v

    -- List membership predicate
    oneOf :: impl Pure v -> [v] -> impl Pure Bool

    -- Conditions
    switch :: [(impl Pure Bool, impl e v)]
           -- ^ List of condition/value pair. The first condition to
           -- be true selects the value of the expression.
           -> impl e v
           -- ^ Default branch (used when no true conditions occured).
           -> impl e v

    -- Verbs with side effects
    setServiceStatus :: IdentI ServiceStatus -> impl Eff ()
    sendDealerMail :: impl Eff ()
    sendGenserMail :: impl Eff ()
    sendPSAMail    :: impl Eff ()
    sendSMS        :: IdentI SmsTemplate -> impl Eff ()

    -- Control flow combinators
    finish  :: impl Eff ActionOutcome
    proceed :: [Action impl] -> impl Eff ActionOutcome
    defer   :: impl Eff ActionOutcome


minutes :: Num i => i
minutes = 60


hours :: Num i => i
hours = 60 * minutes


days :: Num i => i
days = 24 * hours


-- | Action existentially quantified over Backoffice implementations.
type ActionDef = forall impl. Backoffice impl => Action impl
