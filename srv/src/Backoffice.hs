{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Backoffice (
                   -- * Back office DSL and definition
                    Action(..)
                  , Entry(..)
                  , Backoffice(..)
                  , ActionAssignment
                  , ActionOutcome
                  , Trigger
                  , carmaBackoffice

                  -- * Validation
                  , checkBackoffice
                  , backofficeText
                  , backofficeDot
                  , IBox(..)
                  )

where

import           Prelude hiding ((>), (==), (||), (&&), const)
import qualified Prelude as P ((>), (==), (||), (&&), const)

import           Control.Monad.Trans.State
import           Data.Functor

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.BFS
import           Data.GraphViz hiding (fromNode)
import           Data.GraphViz.Printing (printIt)

import           Data.List
import           Data.Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT (Text)
import           Data.Time.Clock
import           Data.Typeable

import           Data.Model
import           Data.Model.Types

import           Carma.Model.ActionResult (ActionResult)
import qualified Carma.Model.ActionResult as AResult
import           Carma.Model.ActionType (ActionType)
import qualified Carma.Model.ActionType as AType
import           Carma.Model.Case.Type as Case
import qualified Carma.Model.CaseStatus as CS
import           Carma.Model.FalseCall as FS
import           Carma.Model.Program as Program
import           Carma.Model.Role as Role
import           Carma.Model.Satisfaction as Satisfaction
import           Carma.Model.Service as Service
import           Carma.Model.ServiceStatus (ServiceStatus)
import qualified Carma.Model.ServiceStatus as SS
import           Carma.Model.ServiceType as ST
import           Carma.Model.SmsTemplate (SmsTemplate)
import qualified Carma.Model.SmsTemplate as SMS
import           Carma.Model.Usermeta (Usermeta)
import qualified Carma.Model.Usermeta as Usermeta


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


data Entry =
    Entry { trigger :: forall impl. (Backoffice impl) => impl Trigger
          , result  :: forall impl. (Backoffice impl) => impl ActionOutcome
          }


toBack :: Entry
toBack =
    Entry
    (Service.status `onServiceField` (const SS.backoffice))
    (([AType.tellMeMore, AType.callMeMaybe] `closeWith` AResult.communicated) *>
     switch
     [ ( serviceField svcType `oneOf` [ST.towage, ST.tech]
       , sendSMS SMS.create *> proceed [AType.orderService]
       )
     , ( serviceField svcType `oneOf` [ST.ken, ST.consultation]
       , sendSMS SMS.complete *>
         setServiceStatus SS.ok *>
         proceed [AType.closeCase, AType.addBill]
       )
     ]
     (proceed [AType.orderServiceAnalyst])
    )


needMakerApproval :: Entry
needMakerApproval =
    Entry
    (Service.status `onServiceField` (const SS.makerApproval))
    (proceed [AType.makerApproval])


needInfo :: Entry
needInfo =
    Entry
    (Case.caseStatus `onCaseField` (const CS.needInfo))
    (proceed [AType.tellMeMore])


complaint :: Entry
complaint =
    Entry
    (Service.clientSatisfied `onServiceField'` (const Satisfaction.none))
    (proceed [AType.complaintResolution])


mistake :: Entry
mistake =
    Entry
    (Service.status `onServiceField` (const SS.mistake))
    finish


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
    (switch
     [ (previousAction == const AType.needPartner, currentUserOr bo_order)
     , (userField Usermeta.isJack, currentUserOr bo_order)
     ]
     (role bo_order)
    )
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
    (role bo_secondary)
    (let
        n = (1 * minutes) `since` now
        t = (1 * days) `before` serviceField' times_expectedServiceStart
     in
       switch [(t > n, t)] ((5 * minutes) `since` now)
    )
    [ (AResult.serviceOrderedAnalyst,
       switch
       [ ( (serviceField svcType == const ST.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , setServiceStatus SS.inProgress *>
           proceed [AType.checkEndOfService, AType.addBill]
         )
       , ( serviceField svcType `oneOf`
           [ ST.taxi
           , ST.sober
           , ST.adjuster
           , ST.insurance
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
    (role bo_control)
    ((5 * minutes) `since` now)
    [ (AResult.clientOk, proceed [AType.checkStatus])
    , (AResult.defer, defer)
    ]


checkStatus :: Action
checkStatus =
    Action
    AType.checkStatus
    (role bo_control)
    ((5 * minutes) `since` serviceField' times_expectedServiceStart)
    [ (AResult.serviceInProgress,
       setServiceStatus SS.inProgress *> proceed [AType.checkEndOfService])
    , (AResult.defer, defer)
    ]


needPartner :: Action
needPartner =
    Action
    AType.needPartner
    (currentUserOr bo_order)
    ((15 * minutes) `since` now)
    [ (AResult.partnerFound,
       setServiceStatus SS.order *> proceed [AType.orderService])
    , (AResult.defer, defer)
    ]

checkEndOfService :: Action
checkEndOfService =
    Action
    AType.checkEndOfService
    (role bo_control)
    ((5 * minutes) `since` serviceField' times_expectedServiceEnd)
    [ (AResult.serviceDone,
       sendSMS SMS.complete *>
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
    (role Role.head)
    ((5 * minutes) `since` now)
    [ (AResult.caseClosed, setServiceStatus SS.closed *> finish)
    , (AResult.defer, defer)
    ]


getDealerInfo :: Action
getDealerInfo =
    Action
    AType.getDealerInfo
    (role bo_dealer)
    (switch
       [ ( (serviceField svcType == const ST.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , (5 * minutes) `since` serviceField' times_factServiceEnd)
       ]
     ((14 * days) `since` serviceField' times_factServiceEnd))
    [ (AResult.gotInfo, sendPSAMail *> finish)
    , (AResult.defer, defer)
    ]


cancelService :: Action
cancelService =
    Action
    AType.cancelService
    (role bo_control)
    ((1 * minutes) `since` now)
    [ (AResult.falseCallUnbilled,
       sendSMS SMS.cancel *>
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall FS.nobill *>
       finish)
    , (AResult.falseCallBilled,
       sendSMS SMS.cancel *>
       setServiceStatus SS.canceled *>
       setServiceField Service.falseCall FS.bill *>
       finish)
    , (AResult.defer, defer)
    ]

makerApproval :: Action
makerApproval =
    Action
    AType.makerApproval
    (role bo_control)
    ((1 * minutes) `since` now)
    [ (AResult.makerApproved, proceed [AType.orderService])
    , (AResult.makerDeclined, proceed [AType.tellMakerDeclined])
    ]


tellMakerDeclined :: Action
tellMakerDeclined =
    Action
    AType.tellMakerDeclined
    (role bo_control)
    ((5 * minutes) `since` now)
    [ (AResult.clientNotified,
       setServiceStatus SS.closed *> finish)
    ]


addBill :: Action
addBill =
    Action
    AType.addBill
    (role bo_bill)
    ((14 * days) `since` now)
    [ (AResult.billAttached, proceed [AType.headCheck])
    , (AResult.returnToBack, proceed [AType.billmanNeedInfo])
    , (AResult.defer, defer)
    ]


billmanNeedInfo :: Action
billmanNeedInfo =
    Action
    AType.billmanNeedInfo
    (role bo_qa)
    ((5 * minutes) `since` now)
    [ (AResult.returnToBillman, proceed [AType.addBill])
    , (AResult.defer, defer)
    ]


headCheck :: Action
headCheck =
    Action
    AType.headCheck
    (role Role.head)
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
    (role bo_director)
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
    (role bo_account)
    ((5 * minutes) `since` now)
    [ (AResult.accountToDirector, proceed [AType.directorCheck])
    , (AResult.confirmedAccount, proceed [AType.analystCheck])
    , (AResult.defer, defer)
    ]


analystCheck :: Action
analystCheck =
    Action
    AType.analystCheck
    (role bo_analyst)
    ((5 * minutes) `since` now)
    [ (AResult.confirmedAnalyst, finish)
    , (AResult.defer, defer)
    ]


complaintResolution :: Action
complaintResolution =
    Action
    AType.complaintResolution
    (role bo_qa)
    ((1 * minutes) `since` now)
    [ (AResult.complaintManaged, finish)
    , (AResult.defer, defer)
    ]


tellMeMore :: Action
tellMeMore =
    Action
    AType.tellMeMore
    (role bo_order)
    ((1 * minutes) `since` now)
    [ (AResult.communicated, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    ]


callMeMaybe :: Action
callMeMaybe =
    Action
    AType.callMeMaybe
    (role bo_order)
    ((1 * minutes) `since` now)
    [ (AResult.complaintManaged, finish)
    , (AResult.okButNoService, finish)
    , (AResult.defer, defer)
    ]


-- | Text embedding for Backoffice DSL types.
newtype TextE t = TextE (TCtx -> Text)


-- | Simple TextE constructor which leaves the context unchanged.
textE :: Text -> TextE t
textE t = TextE (P.const t)


-- | TextE constructor for trigger terms.
triggerText :: TextE a -> TextE v -> TextE t
triggerText field value =
    TextE $ \c ->
        T.concat [ "Когда "
                 , toText c field
                 , " приобретает значение "
                 , toText c value
                 ]


-- | Existential container for model idents.
--
-- Used to store idents for multiple models in a single lookup table.
data IBox = forall m. Model m => IBox (IdentI m)


instance Show IBox where
    show (IBox i) = show i


instance Eq IBox where
    (IBox b1@(Ident i1)) == (IBox b2@(Ident i2)) =
        (typeOf b1, i1) P.== (typeOf b2, i2)


instance Ord IBox where
    compare (IBox b1@(Ident i1)) (IBox b2@(Ident i2)) =
        (typeOf b1, i1) `compare` (typeOf b2, i2)


-- | Context for text embedding (stores mappings from constants to
-- text).
data TCtx = TCtx { identMap :: Map IBox Text
                 }


-- | Convert an ident to text.
lkp :: IBox -> Map IBox Text -> Text
lkp k@(IBox k'@(Ident i)) env =
    maybe
    (T.pack $ show k')
    (\t -> T.concat [t, "#", T.pack $ show i])
    (Map.lookup k env)


instance Backoffice TextE where
    now = textE "Текущее время"
    since dt t =
        TextE (\c -> T.concat [toText c t, " + ", formatDiff dt])
    before dt t =
        TextE (\c -> T.concat [toText c t, " - ", formatDiff dt])

    role r = TextE (\c -> T.append "Пользователи с ролью " $ toText c (const r))
    currentUserOr r =
        TextE $ \c ->
            T.append "Текущий пользователь и другие с ролью " $ toText c (const r)

    previousAction = textE "Предыдущее действие"

    userField     = textE . fieldDesc
    caseField     = textE . fieldDesc
    serviceField  = textE . fieldDesc
    serviceField' = textE . fieldDesc

    onCaseField a v = triggerText (caseField a) v
    onServiceField a v = triggerText (serviceField a) v
    onServiceField' a v = triggerText (serviceField' a) v

    not v =
        TextE (\c -> T.concat ["НЕ выполнено условие ", toText c v])
    a > b =
        TextE (\c -> T.concat [toText c a, " > ", toText c b])
    a == b =
        TextE (\c -> T.concat [toText c a, " равно ", toText c b])
    a && b =
        TextE (\c -> T.concat ["(", toText c a, ") и (", toText c b, ")"])
    a || b =
        TextE (\c -> T.concat ["(", toText c a, ") или (", toText c b, ")"])

    const v = TextE (lkp (IBox v) . identMap)

    oneOf val set =
        TextE $ \c ->
            T.concat [ toText c val
                     , " ∈ {"
                     , T.intercalate "," (map (toText c . const) set)
                     , "}"
                     ]

    switch conds ow =
        TextE $ \c ->
            let
                ppc (cond, act) =
                    T.concat ["Если ", toText c cond, ", то ", toText c act]
            in
              T.concat [ T.intercalate "; " $ Prelude.map ppc conds
                       , "; во всех других случаях — "
                       , toText c ow
                       ]

    setServiceField acc i =
        TextE $ \c ->
            T.concat [fieldDesc acc, " ← ", (toText c . const) i]

    sendSMS i =
        TextE $ \c ->
            T.concat ["Отправить SMS по шаблону ", (toText c . const) i]

    sendPSAMail = textE "Отправить письмо в PSA"

    sendDealerMail = textE "Отправить письмо дилеру"

    sendGenserMail = textE "Отправить письмо в Genser"

    closeWith acts r =
        TextE $ \c ->
            T.concat [ "Закрыть все ранее созданные действия {"
                     , T.intercalate ", " $ map (toText c . const) acts
                     , "} с результатом "
                     , (toText c . const) r
                     ]

    defer = textE "Отложить действие"

    finish = textE "Завершить обработку"

    proceed acts =
        TextE $ \c ->
            T.append "Создать действия: " $
            T.intercalate ", " (map (toText c . const) acts)

    a *> b =
        TextE $ \c ->
        T.concat [toText c a, ", ", toText c b]


-- | Text evaluator for Backoffice DSL.
toText :: TCtx -> TextE v -> Text
toText ctx (TextE f) = f ctx


-- | FGL graph edge embedding. A DSL term is converted to a list of
-- edges depending on all possible outcomes. Only terms with semantic
-- type 'ActionOutcome' are interpreted into non-Nothing values.
-- Chained effects and switch conditions are marked on edge labels
-- with @*@ and @?@ symbols.
--
-- A switch condition results in (b+1) extra edges, where b is the
-- amount of switch branches (counting the default branch). An extra
-- node is included between source and target nodes when a switch
-- construct occurs.
--
-- This embedding is basically a tagged one due to use of Maybe.
-- There're several reasons for this.
--
-- It's unclear what should pure terms produce. One way would be to
-- reinterpret them using text embedding, but by the time an
-- interpreter is selected all types of pure combinators such as
-- 'oneOf' are fixed so that @impl ~ EdgeE@.
--
-- Polymorphic 'switch' is another problem. Switch could combine
-- branches into a list of produced edges (if branches are
-- ActionOutcomes) or a list of strings (for other branches). Without
-- meta-language tags to distinguish terms embedded as node lists and
-- those embedded as strings, it's impossible to write a well-typed
-- combine function.
--
-- The embedding uses both a context to set edge properties and a
-- monad to generate new switch nodes. The two are distinct because
-- the context is not used after the term has been interpreted, while
-- switch node counter is supposed to be used when multiple terms are
-- processed.
data EdgeE t = EdgeE (EdgeCtx -> NodeGenerator (Maybe [LEdge ColoredLabel]))


type ColoredLabel = (Text, Maybe X11Color)


data EdgeCtx = EdgeCtx { fromNode  :: Int
                       , finalNode :: Int
                       -- ^ Final node of the whole state graph.
                       , edgeText  :: [Text]
                       , edgeColor :: Maybe X11Color
                       }


type NodeGenerator a = State Node a


-- | Yield new node index.
mkNewNode :: NodeGenerator Node
mkNewNode = do
  n <- get
  put (n + 1)
  return n


fullEdgeText :: EdgeCtx -> Text
fullEdgeText c = T.intercalate "," $ edgeText c


nothing :: EdgeE t
nothing = EdgeE $ P.const $ return Nothing


instance Backoffice EdgeE where
    now = nothing
    since _ _ = nothing
    before _ _ = nothing

    role _ = nothing
    currentUserOr _ = nothing

    previousAction = nothing

    userField _ = nothing
    caseField _ = nothing
    serviceField _ = nothing
    serviceField' _ = nothing

    onCaseField _ _ = nothing
    onServiceField _ _ = nothing
    onServiceField' _ _ = nothing

    not _ = nothing
    _ > _ = nothing
    _ == _ = nothing
    _ && _ = nothing
    _ || _ = nothing

    switch conds ow =
        EdgeE $ \c ->
            let
                branchColors :: [X11Color]
                branchColors =
                    cycle [ DeepSkyBlue4
                          , DarkOliveGreen
                          , Maroon4
                          , Firebrick
                          , DarkGreen
                          , DarkOrange3
                          , NavyBlue
                          ]
                branches = map snd conds ++ [ow]
                -- Recurse into a switch branch from switch node,
                -- coloring and marking all child edges
                branchToEdge swNode (br, i, col) =
                    toEdge c{ edgeText =
                              [T.append switchLabel $ T.pack $ show i]
                            , edgeColor = Just col
                            , fromNode = swNode} br
            in do
              swNode <- mkNewNode
              -- Insert intermediate switch node between source node
              -- and branch destinations
              let toSwitch = (fromNode c, swNode, (fullEdgeText c, Nothing))
              brs <- mapM (branchToEdge swNode) $
                     zip3 branches [(1::Int)..] branchColors
              return $ Just $ toSwitch:(concat $ catMaybes brs)

    oneOf _ _ = nothing

    const _ = nothing
    setServiceField _ _ = nothing
    sendDealerMail = nothing
    sendGenserMail = nothing
    sendPSAMail = nothing
    sendSMS _ = nothing

    closeWith _ _ = nothing

    defer =
        EdgeE $ \c ->
            return $
            Just [(fromNode c, fromNode c, (fullEdgeText c, edgeColor c))]
    finish =
        EdgeE $ \c ->
            return $
            Just [(fromNode c, finalNode c, (fullEdgeText c, edgeColor c))]
    proceed acts =
        EdgeE $ \c ->
            return $ Just $
            map (\(Ident ai) ->
                 (fromNode c, ai, (fullEdgeText c, edgeColor c))) acts

    -- Mark presence of left-hand effects
    _ *> b = EdgeE $ \c -> toEdge c{edgeText = (edgeText c) ++ ["*"]} b


-- | Interpreter helper to recursively process terms.
toEdge :: EdgeCtx -> EdgeE v -> NodeGenerator (Maybe [LEdge ColoredLabel])
toEdge ctx (EdgeE f) = f ctx


-- | Edge evaluator for DSL.
toEdge' :: EdgeCtx -> EdgeE ActionOutcome -> NodeGenerator [LEdge ColoredLabel]
toEdge' ctx g = fromJust <$> toEdge ctx g


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


-- | Formal description of how a back office operates.
type BackofficeSpec = ([Entry], [Action])


carmaBackoffice :: BackofficeSpec
carmaBackoffice =
    ( [ toBack
      , needInfo
      , needMakerApproval
      , complaint
      , mistake
      ]
    , [ orderService
      , orderServiceAnalyst
      , tellClient
      , checkStatus
      , needPartner
      , checkEndOfService
      , closeCase
      , getDealerInfo
      , cancelService
      , makerApproval
      , tellMakerDeclined
      , addBill
      , billmanNeedInfo
      , headCheck
      , directorCheck
      , accountCheck
      , analystCheck
      , complaintResolution
      , callMeMaybe
      , tellMeMore
      ]
    )


-- | Produce a textual spec from a back office description.
backofficeText :: Map IBox Text -> Text
backofficeText iMap =
    T.unlines $
    ["ВХОДЫ:"] ++
    (indent . concat $ map fmtEntry $ fst carmaBackoffice) ++
    ["ДЕЙСТВИЯ:"] ++
    (indent . concat $ map fmtAction $ snd carmaBackoffice)
    where
      ctx = TCtx iMap
      indent :: [Text] -> [Text]
      indent = map ('\t' `T.cons`)
      fmtEntry e =
          [T.snoc (toText ctx $ trigger e) ':'] ++
          (indent [toText ctx $ result e]) ++
          ["\n"]
      fmtAction a =
          [lkp (IBox $ aType a) iMap] ++
          (indent $
           [ T.concat ["Время выполнения: ", toText ctx $ due a]
           , T.concat ["Ответственность: ", toText ctx $ assignment a]
           , "Результаты:" ] ++
           (indent $
            Prelude.map (\(r, eff) ->
                         T.concat [ lkp (IBox r) iMap
                                  , ": "
                                  , toText ctx eff
                                  ]) $
            outcomes a)
          ) ++
          ["\n"]


-- Internal ActionType-like code for graph start and node. Used only
-- when a back office graph is analyzed or printed. Actions of this
-- type are never actually created. No ActionType ident must collide
-- with any of these ids.
startNode :: LNode Text
startNode = (-1, "START")

finishNode :: LNode Text
finishNode = (0, "FINISH")


switchLabel :: Text
switchLabel = "?"


-- | FGL interface. Produce labeled nodes and edges from a back office
-- description.
--
-- Non-switch node indices correspond to numeric values of
-- corresponding ActionType idents.
--
-- Extra finish & start nodes are explicitly inserted into the graph.
-- Node indices of 'finish' and 'start' must not be used by any of
-- other idents.
--
-- Switch nodes are also added for every switch construct on an edge.
backofficeNodesEdges :: Map IBox Text -> ([LNode Text], [LEdge ColoredLabel])
backofficeNodesEdges iMap =
    ( stateNodes ++ switchNodes
    , allEdges
    )
    where
      -- First, build graph nodes for states (action types plus
      -- start/finish states)
      stateNodes :: [LNode Text]
      stateNodes = startNode:
                   finishNode:
                   (map mkNode $ snd carmaBackoffice)
      mkNode :: Action -> LNode Text
      mkNode a = (i, lkp (IBox t) iMap)
          where
            t@(Ident i) = aType a
      -- Find out first untaken node id and use it to produce switch
      -- nodes
      firstSwitchNode :: Node
      firstSwitchNode = 1 + (maximum $ map fst stateNodes)
      -- Generate all edges. Count and generate extra nodes produced
      -- for switch constructs.
      (allEdges, nextSwitchNode) = runState mkEdges firstSwitchNode
      switchNodes :: [LNode Text]
      switchNodes = zip [firstSwitchNode .. nextSwitchNode - 1] $
                    repeat switchLabel
      mkEdges :: NodeGenerator [LEdge ColoredLabel]
      mkEdges = do
        entries <- mapM mkEntryEdges $ fst carmaBackoffice
        results <- mapM mkResultEdges $ snd carmaBackoffice
        return $ concat $ entries ++ results
      mkEntryEdges :: Entry -> NodeGenerator [LEdge ColoredLabel]
      mkEntryEdges e =
          toEdge' (EdgeCtx
                   (fst startNode)
                   (fst finishNode)
                   ["T"]
                   Nothing) $ result e
      mkResultEdges :: Action -> NodeGenerator [LEdge ColoredLabel]
      mkResultEdges a = do
        let Ident i = aType a
        concat <$> (mapM
                    (\(r, o) ->
                     toEdge' (EdgeCtx
                              i
                              (fst finishNode)
                              [lkp (IBox r) iMap]
                              Nothing) o) $
                    outcomes a)


backofficeGraph :: Map IBox Text -> Gr Text ColoredLabel
backofficeGraph iMap = uncurry mkGraph (backofficeNodesEdges iMap)


-- | Produce GraphViz .dot code.
backofficeDot :: Map IBox Text -> LT.Text
backofficeDot iMap =
    printIt $
    graphToDot nonClusteredParams{ fmtNode = fmtN
                                 , fmtEdge = fmtE} $
    backofficeGraph iMap
    where
      fmtE = \(_, _, (l, c)) ->
             [ toLabel l
             , color $ fromMaybe Black c]
      fmtN = \n@(_, l) ->
             [ toLabel l
             , shape $
               if (n P.== finishNode) P.|| (n P.== startNode)
               then DoubleCircle
               else if l P.== switchLabel
                    then DiamondShape
                    else Ellipse
             ]


-- | A critical flaw in back office.
data ValidityError = OutOfGraphTarget (ActionTypeI, ActionTypeI)
                   -- ^ The edge leads to a node not described in the
                   -- graph.
                   | Trap (ActionTypeI)
                   -- ^ The node has no path to finish node.
                   | Unreachable (ActionTypeI)
                   -- ^ The node has no path from start node.
                   | DuplicateNode ActionTypeI
                   -- ^ A node is described more than once.
                   | ExplicitStart
                   -- ^ Start node explicitly mentioned (ident
                   -- collision).
                   | ExplicitFinish
                   -- ^ Finish node explicitly mentioned (ident
                   -- collision).
                     deriving Show


-- | Run back office validity checks.
--
-- Our back office description uses indirect addressing, which may
-- lead to graph consistency errors untraceable on type level. Another
-- source of errors is ident mapping (multiple action types may be
-- accidentally assigned the same numeric id, or some of the magic
-- id's may be referred).
--
-- If this returns non-null, the back office cannot be used.
checkBackoffice :: Map IBox Text -> [ValidityError]
checkBackoffice iMap =
    -- Check dupes
    map DuplicateNode (origNodes \\ uniqNodes) ++
    -- Detect traps
    map Trap (filter (\(Ident n) ->
                      null $ esp n finishId graph) origNodes) ++
    -- Detect unreachable nodes
    map Unreachable (filter (\(Ident n) ->
                      null $ esp startId n graph) origNodes) ++
    -- Check START/FINISH collisions
    if Ident finishId `elem` origNodes
    then [ExplicitFinish] else [] ++
    if Ident startId `elem` origNodes
    then [ExplicitStart] else [] ++
    -- Check unknown outcomes
    outs
    where
      finishId = fst finishNode
      startId = fst startNode
      origNodes = map aType $ snd carmaBackoffice
      uniqNodes = nub origNodes
      (_, edges') = backofficeNodesEdges iMap
      graph = backofficeGraph iMap
      outs = map OutOfGraphTarget $
             mapMaybe (\(from, to, _) ->
                       if or [ Ident to `elem` origNodes
                             , to P.== finishId
                             ]
                       then Nothing
                       else Just (Ident from, Ident to)) edges'
