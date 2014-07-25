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

import           Data.Text as T hiding (concatMap, filter, map, zip)

import           Data.Time.Clock

import           Data.Model
import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import qualified Carma.Model.ActionResult as AResult
import           Carma.Model.ActionType (ActionType)
import qualified Carma.Model.ActionType as AType
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
data Action =
    Action { aType      :: IdentI ActionType
           -- ^ Binds action definition to its name and
           -- description as stored in the database.
           , targetRole :: IdentI Role
           -- ^ Default target role for newly created actions
           -- of this type.
           , due        :: forall impl. (Backoffice impl) =>
                           impl Pure UTCTime
           -- ^ Default due time for new actions.
           , outcomes   :: forall impl. (Backoffice impl) =>
                           [(IdentI ActionResult, impl Eff ActionOutcome)]
           -- ^ All action results (outward node edges).
           }


-- | Forces us to use one of the action-ending verbs from Backoffice
-- DSL.
data ActionOutcome


-- | Type tag for effect control. Pure and Eff are lifted to type
-- level.
data Effects = Pure | Eff


-- | Back office language (typed tagless final representation).
class Backoffice impl where
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

    -- Branching (preserves effect typing)
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
    proceed :: [IdentI ActionType] -> impl Eff ActionOutcome
    defer   :: impl Eff ActionOutcome

    -- Action chains
    infixr *>
    (*>) :: impl Eff () -> impl Eff ActionOutcome -> impl Eff ActionOutcome


minutes :: Num i => i
minutes = 60


hours :: Num i => i
hours = 60 * minutes


days :: Num i => i
days = 24 * hours


orderService :: Action
orderService =
    Action
    AType.orderService
    Role.bo_order
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(t > n, t)] ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrdered,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [AType.tellClient, AType.addBill])
    , (AResult.serviceOrderedSMS,
       sendSMS SMS.order *>
       sendPSAMail *>
       setServiceStatus SS.ordered *>
       proceed [AType.checkStatus, AType.addBill])
    , (AResult.needPartner,
       sendSMS SMS.parguy *>
       setServiceStatus SS.needPartner *>
       proceed [AType.needPartner])
    , (AResult.clientCanceledService,
       sendSMS SMS.cancel *>
       sendPSAMail *>
       setServiceStatus SS.canceled *>
       finish)
    , (AResult.defer, defer)
    ]


orderServiceAnalyst :: Action
orderServiceAnalyst =
    Action
    AType.orderServiceAnalyst
    Role.bo_secondary
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(t > n, t)] ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrderedAnalyst,
       switch
       [ ( (serviceField svcType == const ServiceType.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , setServiceStatus SS.inProgress *>
           proceed [AType.checkEndOfService, AType.addBill]
         )
       , ( serviceField svcType `oneOf`
           [ ServiceType.taxi
           , ServiceType.sober
           , ServiceType.adjuster
           , ServiceType.insurance
           ]
         , setServiceStatus SS.ordered *>
           proceed [AType.checkStatus, AType.addBill])
       ]
       (setServiceStatus SS.ordered *>
        proceed [AType.closeCase, AType.addBill]))
    , (AResult.defer, defer)
    ]


tellClient :: Action
tellClient =
    Action
    AType.tellClient
    Role.bo_control
    ((5 * minutes) `since` now)
    [ (AResult.clientOk, proceed [AType.checkStatus])
    , (AResult.defer, defer)
    ]


checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    Role.bo_control
    ((5 * minutes) `since` serviceField' times_expectedServiceStart)
    [ (AResult.serviceInProgress, defer)
    , (AResult.defer, defer)
    ]


needPartner :: Action
needPartner =
    Action
    AType.needPartner
    Role.bo_order
    ((15 * minutes) `since` now)
    [ (AResult.partnerFound, proceed [AType.orderService])
    , (AResult.defer, defer)
    ]

checkEndOfService :: Action
checkEndOfService =
    Action
    AType.checkEndOfService
    Role.bo_control
    ((5 * minutes) `since` serviceField' times_expectedServiceEnd)
    [ (AResult.serviceDone,
       sendSMS SMS.cancel *>
       sendDealerMail *>
       setServiceStatus SS.ok *>
       switch [( caseField Case.program `oneOf`
                 [Program.peugeot, Program.citroen, Program.vw]
               , proceed [AType.closeCase, AType.getDealerInfo])
              ]
              (proceed [AType.closeCase]))
    , (AResult.defer, defer)
    ]


closeCase :: Action
closeCase =
    Action
    AType.closeCase
    Role.head
    ((5 * minutes) `since` now)
    [ (AResult.caseClosed, setServiceStatus SS.closed *> finish)
    , (AResult.defer, defer)
    ]


getDealerInfo :: Action
getDealerInfo =
    Action
    AType.getDealerInfo
    Role.bo_dealer
    ((14 * days) `since` now)
    [ (AResult.gotInfo, sendPSAMail *> finish)
    , (AResult.defer, defer)
    ]


makerApproval :: Action
makerApproval =
    Action
    AType.makerApproval
    Role.bo_control
    ((1 * minutes) `since` now)
    [ (AResult.makerApproved, proceed [AType.orderService])
    , (AResult.makerDeclined, proceed [AType.tellMakerDeclined])
    ]


tellMakerDeclined :: Action
tellMakerDeclined =
    Action
    AType.tellMakerDeclined
    Role.bo_control
    ((5 * minutes) `since` now)
    [ (AResult.clientNotified,
       setServiceStatus SS.closed *> proceed [AType.orderService])
    ]


addBill :: Action
addBill =
    Action
    AType.addBill
    Role.bo_bill
    ((7 * days) `since` now)
    [ (AResult.billAttached, proceed [AType.headCheck])
    , (AResult.returnToBack, proceed [AType.billmanNeedInfo])
    , (AResult.defer, defer)
    ]


billmanNeedInfo :: Action
billmanNeedInfo =
    Action
    AType.billmanNeedInfo
    Role.bo_qa
    ((5 * minutes) `since` now)
    [ (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    ]


headCheck :: Action
headCheck =
    Action
    AType.headCheck
    Role.head
    ((5 * minutes) `since` now)
    [ (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.confirmedWODirector, proceed [AType.accountCheck])
    , (AResult.confirmedHead, proceed [AType.directorCheck])
    , (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    ]


directorCheck :: Action
directorCheck =
    Action
    AType.directorCheck
    Role.bo_director
    ((5 * minutes) `since` now)
    [ (AResult.directorToHead, proceed [AType.headCheck])
    , (AResult.confirmedDirector, proceed [AType.accountCheck])
    , (AResult.confirmedFinal, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    ]


accountCheck :: Action
accountCheck =
    Action
    AType.accountCheck
    Role.bo_account
    ((5 * minutes) `since` now)
    [ (AResult.accountToDirector, proceed [AType.directorCheck])
    , (AResult.confirmedAccount, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    ]


analystCheck :: Action
analystCheck =
    Action
    AType.analystCheck
    Role.bo_analyst
    ((5 * minutes) `since` now)
    [ (AResult.confirmedAnalyst, finish)
    , (AResult.defer, defer)
    ]


complaintResolution :: Action
complaintResolution =
    Action
    AType.complaintResolution
    Role.bo_qa
    ((1 * minutes) `since` now)
    [ (AResult.complaintManaged, finish)
    , (AResult.defer, defer)
    ]


-- | Text embedding for DSL types.
newtype TextE (a :: Effects) t = TextE Text


instance Backoffice TextE where
    now = TextE "Текущее время"
    since dt t =
        TextE $ T.concat [toText t, " + ", formatDiff dt]
    before dt t =
        TextE $ T.concat [toText t, " - ", formatDiff dt]

    caseField     = TextE . fieldDesc
    caseField'    = TextE . fieldDesc
    serviceField  = TextE . fieldDesc
    serviceField' = TextE . fieldDesc

    not v = TextE $ T.concat ["НЕ выполнено условие ", toText v]
    a > b = TextE $ T.concat [toText a, " > ", toText b]
    a == b = TextE $ T.concat [toText a, " равно ", toText b]
    a && b = TextE $ T.concat ["(", toText a, ") и (", toText b, ")"]
    a || b = TextE $ T.concat ["(", toText a, ") или (", toText b, ")"]

    const = TextE . T.pack . show

    oneOf val set =
        TextE $
        T.concat [toText val
                 , " ∈ {"
                 , T.intercalate "," (map (T.pack . show) set)
                 , "}"
                 ]

    switch conds ow =
        TextE $ T.concat
        [ T.intercalate "; " $ Prelude.map pp conds
        , "; во всех других случаях — "
        , toText ow
        ]
        where
          pp (cond, act) = T.concat ["Если ", toText cond, ", то ", toText act]

    setServiceStatus i =
        TextE $ T.concat [fieldDesc Service.status, " ← ", T.pack $ show i]

    sendSMS (Ident i) =
        TextE $ T.concat ["Отправить SMS по шаблону №", T.pack $ show i]

    sendPSAMail = TextE "Отправить письмо в PSA"

    sendDealerMail = TextE "Отправить письмо дилеру"

    sendGenserMail = TextE "Отправить письмо в Genser"

    defer = TextE "Отложить действие"

    finish = TextE "Завершить обработку"

    proceed acts =
        TextE $ T.append "Создать действия: " $
        T.intercalate "," (Prelude.map (toText . const) acts)

    a *> b = TextE $ T.concat [toText a, ", ", toText b]


-- | Text evaluator for DSL.
toText :: TextE e v -> Text
toText (TextE t) = t


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
    map (fmtAction) [ orderService
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
    where
      formatAType :: IdentI ActionType -> Text
      formatAType = T.pack . show
      formatAResult :: IdentI ActionResult -> Text
      formatAResult = T.pack . show
      indent :: [Text] -> [Text]
      indent = map ("\t" `T.append`)
      fmtAction a =
          T.unlines $
               [formatAType $ aType a] ++
               (indent $
                [ T.concat ["Время выполнения: ", toText $ due a]
                , "Результаты:" ] ++
                (indent $
                 Prelude.map (\(r, eff) ->
                              T.concat [ formatAResult r
                                       , ": "
                                       , toText eff
                                       ]) $
                 outcomes a)
               )
