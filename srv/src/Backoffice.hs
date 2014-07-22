{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Backoffice (backofficeGraph)

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


orderService :: ActionDef
orderService =
    Action
    ActionType.orderService
    Role.bo_order
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(Backoffice.not $ t > n, t)] ((5 * minutes) `since` now)
    )
    [ (ActionResult.serviceOrdered,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [tellClient, addBill])
    , (ActionResult.serviceOrderedSMS,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [checkStatus, addBill])
    , (ActionResult.needPartner,
       sendSMS SMS.parguy *>
       setServiceStatus SS.needPartner *>
       proceed [needPartner])
    , (ActionResult.clientCanceledService,
       sendSMS SMS.cancel *>
       sendPSAMail *>
       setServiceStatus SS.canceled *>
       finish)
    , (ActionResult.defer, defer)
    ]


orderServiceAnalyst :: ActionDef
orderServiceAnalyst =
    Action
    ActionType.orderServiceAnalyst
    Role.bo_secondary
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(Backoffice.not $ t > n, t)] ((5 * minutes) `since` now)
    )
    [ (ActionResult.serviceOrderedAnalyst,
       switch
       [ ( (serviceField svcType == const ServiceType.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , setServiceStatus SS.inProgress *>
           proceed [checkEndOfService, addBill]
         )
       , ( serviceField svcType `oneOf`
           [ ServiceType.taxi
           , ServiceType.sober
           , ServiceType.adjuster
           , ServiceType.insurance
           ]
         , setServiceStatus SS.ordered *>
           proceed [checkStatus, addBill])
       ]
       (setServiceStatus SS.ordered *> proceed [closeCase, addBill]))
    , (ActionResult.defer, defer)
    ]


tellClient :: ActionDef
tellClient =
    Action
    ActionType.tellClient
    Role.bo_control
    ((5 * minutes) `since` now)
    [ (ActionResult.clientOk, proceed [checkStatus])
    , (ActionResult.defer, defer)
    ]


checkStatus :: ActionDef
checkStatus =
    Action
    ActionType.checkStatus
    Role.bo_control
    ((5 * minutes) `since` serviceField' times_expectedServiceStart)
    [ (ActionResult.serviceInProgress, defer)
    , (ActionResult.defer, defer)
    ]


needPartner :: ActionDef
needPartner =
    Action
    ActionType.needPartner
    Role.bo_order
    ((15 * minutes) `since` now)
    [ (ActionResult.partnerFound, proceed [orderService])
    , (ActionResult.defer, defer)
    ]

checkEndOfService :: ActionDef
checkEndOfService =
    Action
    ActionType.checkEndOfService
    Role.bo_control
    ((5 * minutes) `since` serviceField' times_expectedServiceEnd)
    [ (ActionResult.serviceDone,
       sendSMS SMS.cancel *>
       sendDealerMail *>
       setServiceStatus SS.ok *>
       switch [( caseField Case.program `oneOf`
                 [Program.peugeot, Program.citroen, Program.vw]
               , proceed [closeCase, getDealerInfo])
              ]
              (proceed [closeCase]))
    , (ActionResult.defer, defer)
    ]


closeCase :: ActionDef
closeCase =
    Action
    ActionType.closeCase
    Role.head
    ((5 * minutes) `since` now)
    [ (ActionResult.caseClosed, setServiceStatus SS.closed *> finish)
    , (ActionResult.defer, defer)
    ]


getDealerInfo :: ActionDef
getDealerInfo =
    Action
    ActionType.getDealerInfo
    Role.bo_dealer
    ((14 * days) `since` now)
    [ (ActionResult.gotInfo, sendPSAMail *> finish)
    , (ActionResult.defer, defer)
    ]


makerApproval :: ActionDef
makerApproval =
    Action
    ActionType.makerApproval
    Role.bo_control
    ((1 * minutes) `since` now)
    [ (ActionResult.makerApproved, proceed [orderService])
    , (ActionResult.makerDeclined, proceed [tellMakerDeclined])
    ]


tellMakerDeclined :: ActionDef
tellMakerDeclined =
    Action
    ActionType.tellMakerDeclined
    Role.bo_control
    ((5 * minutes) `since` now)
    [ (ActionResult.clientNotified,
       setServiceStatus SS.closed *> proceed [orderService])
    ]


addBill :: ActionDef
addBill =
    Action
    ActionType.addBill
    Role.bo_bill
    ((7 * days) `since` now)
    [ (ActionResult.billAttached, proceed [headCheck])
    , (ActionResult.returnToBack, proceed [billmanNeedInfo])
    , (ActionResult.defer, defer)
    ]


billmanNeedInfo :: ActionDef
billmanNeedInfo =
    Action
    ActionType.billmanNeedInfo
    Role.bo_qa
    ((5 * minutes) `since` now)
    [ (ActionResult.returnToBillman, proceed [addBill])
    , (ActionResult.defer, defer)
    ]


headCheck :: ActionDef
headCheck =
    Action
    ActionType.headCheck
    Role.head
    ((5 * minutes) `since` now)
    [ (ActionResult.confirmedFinal, proceed [analystCheck])
    , (ActionResult.confirmedWODirector, proceed [accountCheck])
    , (ActionResult.confirmedHead, proceed [directorCheck])
    , (ActionResult.returnToBillman, proceed [addBill])
    , (ActionResult.defer, defer)
    ]


directorCheck :: ActionDef
directorCheck =
    Action
    ActionType.directorCheck
    Role.bo_director
    ((5 * minutes) `since` now)
    [ (ActionResult.directorToHead, proceed [headCheck])
    , (ActionResult.confirmedDirector, proceed [accountCheck])
    , (ActionResult.confirmedFinal, proceed [analystCheck])
    , (ActionResult.defer, defer)
    ]


accountCheck :: ActionDef
accountCheck =
    Action
    ActionType.accountCheck
    Role.bo_account
    ((5 * minutes) `since` now)
    [ (ActionResult.accountToDirector, proceed [directorCheck])
    , (ActionResult.confirmedAccount, proceed [analystCheck])
    , (ActionResult.defer, defer)
    ]


analystCheck :: ActionDef
analystCheck =
    Action
    ActionType.analystCheck
    Role.bo_analyst
    ((5 * minutes) `since` now)
    [ (ActionResult.confirmedAnalyst, finish)
    , (ActionResult.defer, defer)
    ]


complaintResolution :: ActionDef
complaintResolution =
    Action
    ActionType.complaintResolution
    Role.bo_qa
    ((1 * minutes) `since` now)
    [ (ActionResult.complaintManaged, finish)
    , (ActionResult.defer, defer)
    ]


-- | Graphviz embedding for DSL types.
data GraphE (a :: Effects) t = GraphE Text


-- We ignore actual values in Graphviz.
instance Functor (GraphE Eff) where
    fmap f a = GraphE $ toGraph a


instance Applicative (GraphE Eff) where
    pure a = GraphE ""
    a <*> b = GraphE $ T.concat [toGraph a, ", ", toGraph b]


-- | Graphviz interpreter.
instance Backoffice GraphE where
    now = GraphE "Текущее время"
    since dt t =
        GraphE $ T.concat [toGraph t, " + ", formatDiff dt]
    before dt t =
        GraphE $ T.concat [toGraph t, " - ", formatDiff dt]

    caseField     = GraphE . fieldDesc
    caseField'    = GraphE . fieldDesc
    serviceField  = GraphE . fieldDesc
    serviceField' = GraphE . fieldDesc

    not v = GraphE $ T.concat ["НЕ выполнено условие ", toGraph v]
    a > b = GraphE $ T.concat [toGraph a, " > ", toGraph b]
    a == b = GraphE $ T.concat [toGraph a, " равно ", toGraph b]
    a && b = GraphE $ T.concat ["(", toGraph a, ") и (", toGraph b, ")"]
    a || b = GraphE $ T.concat ["(", toGraph a, ") или (", toGraph b, ")"]

    switch conds ow =
        GraphE $ T.concat
        [ T.intercalate ",\n" $ Prelude.map f conds
        , ",\nво всех других случаях — "
        , toGraph ow
        ]
        where
          f (cond, act) = T.concat ["Если ", toGraph cond, ", то ", toGraph act]

    setServiceStatus i =
        GraphE $ T.concat [fieldDesc Service.status, " ← ", T.pack $ show i]

    sendSMS (Ident i) =
        GraphE $ T.concat ["Отправить SMS по шаблону №", T.pack $ show i]

    sendPSAMail = GraphE "Отправить письмо в PSA"

    sendDealerMail = GraphE "Отправить письмо дилеру"

    sendGenserMail = GraphE "Отправить письмо в Genser"


-- | Graphviz evaluator for DSL.
toGraph :: GraphE e v -> Text
toGraph (GraphE t) = t


-- | Show non-zero days, hours, minutes and seconds of a time
-- difference.
formatDiff :: NominalDiffTime -> Text
formatDiff nd' =
    let
        nd :: Int
        nd = round nd'
        totalDays = nd `div` days
        r1 = nd - totalDays * days
        totalHours = r1 `div` hours
        r2 = r1 - totalHours * hours
        totalMins = r2 `div` minutes
        totalSecs = (r2 - totalMins * minutes)
        labels = zip
                 [totalDays, totalHours, totalMins, totalSecs]
                 ["д", "ч", "м", "с"]
        nonZeros = filter (\(v, _) -> v /= 0) labels
    in
      T.pack $ concatMap (\(v, l) -> show v ++ l) nonZeros


-- | WIP
backofficeGraph :: Text
backofficeGraph =
    T.unlines $
    map (toGraph .due) [ orderService
                       , orderServiceAnalyst
                       , tellClient
                       , checkStatus
                       , needPartner
                       , checkEndOfService
                       , closeCase
                       , getDealerInfo
                       , makerApproval
                       , tellMakerDeclined
                       , addBill
                       , billmanNeedInfo
                       , headCheck
                       , directorCheck
                       , accountCheck
                       , analystCheck
                       , complaintResolution
                       ]
