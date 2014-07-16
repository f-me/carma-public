{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}

module Backoffice where

import           Prelude hiding ((>), (==), (||), (&&))
import qualified Prelude ((>), (==), (||), (&&))

import           Data.Functor
import           Data.Maybe

import           Data.Time.Clock

import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import qualified Carma.Model.ActionResult as ActionResult
import           Carma.Model.ActionType (ActionType)
import qualified Carma.Model.ActionType as ActionType
import           Carma.Model.Case.Type as Case
import           Carma.Model.Role as Role
import           Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import qualified Carma.Model.ServiceStatus as SS
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
           , due        :: impl UTCTime
           -- ^ Default due time for new actions. TODO: This should
           -- live in a subset of impl to avoid side effects.
           , outcomes   :: [(IdentI ActionResult, impl ActionOutcome)]
           -- ^ All action results (outward node edges).
           }


-- | Forces us to use one of the action-ending verbs from Backoffice
-- DSL.
data ActionOutcome


-- | Back office language (tagless final embedding).
class (Functor impl, Monad impl) => Backoffice impl where
    -- Time helpers (see also 'minutes', 'hours', 'days').
    now    :: impl UTCTime
    since  :: NominalDiffTime -> impl UTCTime -> impl UTCTime
    before :: NominalDiffTime -> impl UTCTime -> impl UTCTime
    before diff time = (-diff) `since` time

    -- Context access
    caseField        :: (Case -> F t n d) -> impl t
    serviceField     :: (Service -> F t n d) -> impl t
    setServiceStatus :: IdentI ServiceStatus -> impl ()

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

    -- List elem replacement
    oneOf :: impl v -> [v] -> impl Bool

    -- Conditions
    switch :: [(impl Bool, impl v)] -> impl v

    pure :: v -> impl v
    pure = return

    sendDealerMail :: impl ()
    sendGenserMail :: impl ()
    sendPSAMail    :: impl ()

    sendSMS :: IdentI SmsTemplate -> impl ()

    finish  :: impl ActionOutcome
    proceed :: [Action impl] -> impl ActionOutcome
    defer   :: impl ActionOutcome


minutes :: NominalDiffTime
minutes = 60


hours :: NominalDiffTime
hours = 60 * minutes


days :: NominalDiffTime
days = 24 * hours


-- | Action existentially quantified over Backoffice implementations.
type ActionDef = forall impl. Backoffice impl => Action impl


order :: ActionDef
order = Action
        ActionType.orderService
        Role.bo_order
        (let
            n = (1 * minutes) `since` now
            s = fromJust <$> serviceField times_expectedServiceStart
            t = (1 * days) `before` s
         in
           switch [ (t > n || t == n, n)
                  , (n > t, t)
                  ]
        )
        [ (ActionResult.serviceOrdered,
           do
             sendSMS SMS.order
             sendPSAMail
             setServiceStatus SS.ordered
             proceed [tellClient, addBill])
        , (ActionResult.serviceOrderedSMS,
           do
             sendSMS SMS.order
             sendPSAMail
             setServiceStatus SS.ordered
             proceed [checkStatus, addBill])
        , (ActionResult.needPartner,
           do
             sendSMS SMS.parguy
             setServiceStatus SS.needPartner
             proceed [needPartner])
        , (ActionResult.clientCanceledService,
           do
             sendSMS SMS.cancel
             sendPSAMail
             setServiceStatus SS.canceled
             finish)
        , (ActionResult.defer, defer)
        ]


orderServiceAnalyst :: ActionDef
orderServiceAnalyst = Action
                      ActionType.orderServiceAnalyst
                      Role.bo_secondary
                      ((5 * minutes) `since` now)
                      [ (ActionResult.defer, defer) ]


tellClient :: ActionDef
tellClient = Action
             ActionType.tellClient
             Role.bo_control
             ((5 * minutes) `since` now)
             [ (ActionResult.clientOk, proceed [checkStatus])
             , (ActionResult.defer, defer)
             ]


checkStatus :: ActionDef
checkStatus = Action
              ActionType.checkStatus
              Role.bo_control
              ((5 * minutes) `since`
               (fromJust <$> serviceField times_expectedServiceStart))
              [ (ActionResult.serviceInProgress, defer)
              , (ActionResult.defer, defer)
              ]

needPartner :: ActionDef
needPartner = Action
              ActionType.needPartner
              Role.bo_order
              ((15 * minutes) `since` now)
              [ (ActionResult.partnerFound, proceed [order])
              , (ActionResult.defer, defer)
              ]


addBill :: ActionDef
addBill = Action
          ActionType.addBill
          Role.bo_bill
          ((7 * days) `since` now)
          [ (ActionResult.billAttached, proceed [headCheck])
          , (ActionResult.returnToBack, proceed [billmanNeedInfo])
          , (ActionResult.defer, defer)
          ]


headCheck :: ActionDef
headCheck = Action
            ActionType.headCheck
            Role.head
            ((5 * minutes) `since` now)
            [ (ActionResult.defer, defer) ]


billmanNeedInfo :: ActionDef
billmanNeedInfo = Action
                  ActionType.billmanNeedInfo
                  Role.bo_qa
                  ((5 * minutes) `since` now)
                  [ (ActionResult.returnToBillman, proceed [addBill])
                  , (ActionResult.defer, defer )]


-- graphedActions :: [Action Graph]
-- graphedActions = [ orderServiceAnalyst
--                  , tellClient
--                  , checkStatus
--                  , needPartner
--                  , addBill
--                  , headCheck
--                  , billmanNeedInfo
--                  ]
