{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Backoffice ( backofficeText
                  , backofficeDot
                  , IBox(..)
                  )
where

import           Prelude hiding ((>), (==), (||), (&&), const)
import qualified Prelude ((>), (==), (||), (&&), const)


import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.GraphViz hiding (fromNode)
import           Data.GraphViz.Printing (printIt)

import Data.Maybe
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

    -- Lift idents for use with comparison combinators
    const :: Model v =>
             IdentI v -> impl Pure (IdentI v)

    -- List membership predicate
    oneOf :: Model v =>
             impl Pure (IdentI v) -> [IdentI v] -> impl Pure Bool

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
    [ (AResult.serviceInProgress, proceed [AType.checkEndOfService])
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
    (switch
       [ ( (serviceField svcType == const ServiceType.rent) &&
           caseField Case.program `oneOf` [Program.peugeot, Program.citroen]
         , (5 * minutes) `since` serviceField' times_factServiceEnd)
       ]
     ((14 * days) `since` serviceField' times_factServiceEnd))
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
       setServiceStatus SS.closed *> finish)
    ]


addBill :: Action
addBill =
    Action
    AType.addBill
    Role.bo_bill
    ((14 * days) `since` now)
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
newtype TextE (a :: Effects) t = TextE (TCtx -> Text)


-- | Simple TextE constructor which leaves the context unchanged.
textE :: Text -> TextE a t
textE t = TextE (Prelude.const t)



-- | Existential container for model idents.
--
-- Used to store idents for multiple models in a single lookup table.
data IBox = forall m. Model m => IBox (IdentI m)


instance Show IBox where
    show (IBox i) = show i


instance Eq IBox where
    (IBox b1@(Ident i1)) == (IBox b2@(Ident i2)) =
        (typeOf b1, i1) Prelude.== (typeOf b2, i2)


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

    caseField     = textE . fieldDesc
    caseField'    = textE . fieldDesc
    serviceField  = textE . fieldDesc
    serviceField' = textE . fieldDesc

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

    setServiceStatus i =
        TextE $ \c ->
            T.concat [fieldDesc Service.status, " ← ", (toText c . const) i]

    sendSMS i =
        TextE $ \c ->
            T.concat ["Отправить SMS по шаблону ", (toText c . const) i]

    sendPSAMail = textE "Отправить письмо в PSA"

    sendDealerMail = textE "Отправить письмо дилеру"

    sendGenserMail = textE "Отправить письмо в Genser"

    defer = textE "Отложить действие"

    finish = textE "Завершить обработку"

    proceed acts =
        TextE $ \c ->
            T.append "Создать действия: " $
            T.intercalate ", " (map (toText c . const) acts)

    a *> b =
        TextE $ \c ->
        T.concat [toText c a, ", ", toText c b]


-- | Text evaluator for DSL.
toText :: TCtx -> TextE e v -> Text
toText ctx (TextE f) = f ctx


-- | GraphE (fgl graph edge) embedding doesn't fit well in tagless
-- world. Thus we allow some terms to produce no result after
-- interpreting, efficiently *tagging* results using Maybe
-- constructors.
data GraphE (e :: Effects) t = GraphE (GraphCtx -> Maybe [LEdge Text])


data GraphCtx = GraphCtx { fromNode  :: Int
                         , finalNode :: Int
                         , result    :: IdentI ActionResult
                         , edgeLabel :: [Text]
                         -- ^ Extra text for edge label.
                         , tCtx      :: TCtx
                         }


fullEdgeLabel :: GraphCtx -> Text
fullEdgeLabel c =
    T.concat [ resultLabel
             , if null (edgeLabel c)
               then ""
               else ": " `T.append` (T.intercalate ", " $ edgeLabel c)
             ]
    where
      resultLabel = lkp (IBox $ result c) $ identMap $ tCtx c


nothing :: GraphE e t
nothing = GraphE $ Prelude.const Nothing


instance Backoffice GraphE where
    now = nothing
    since _ _ = nothing
    before _ _ = nothing

    caseField _ = nothing
    caseField' _ = nothing
    serviceField _ = nothing
    serviceField' _ = nothing

    not _ = nothing
    _ > _ = nothing
    _ == _ = nothing
    _ && _ = nothing
    _ || _ = nothing

    switch conds ow =
        GraphE $ \c ->
            let
                toGraph' = toGraph c{edgeLabel="?":(edgeLabel c)}
            in
              Just $ concat $ catMaybes $
                       (toGraph' ow):(map (toGraph' . snd) conds)

    oneOf _ _ = nothing

    const _ = nothing
    setServiceStatus _ = nothing
    sendDealerMail = nothing
    sendGenserMail = nothing
    sendPSAMail = nothing
    sendSMS _ = nothing

    defer = GraphE (\c -> Just [(fromNode c, fromNode c, fullEdgeLabel c)])
    finish = GraphE (\c -> Just [(fromNode c, finalNode c, fullEdgeLabel c)])
    proceed acts =
        GraphE $ \c ->
            Just $ map (\(Ident ai) -> (fromNode c, ai, fullEdgeLabel c)) acts

    -- Mark presence of left-hand effects
    _ *> b = GraphE $ \c -> toGraph c{edgeLabel = "*":(edgeLabel c)} b


toGraph :: GraphCtx -> GraphE e v -> Maybe [LEdge Text]
toGraph ctx (GraphE f) = f ctx


toEdge :: GraphCtx -> GraphE Eff ActionOutcome -> [LEdge Text]
toEdge ctx g =
    case toGraph ctx g of
      Just v -> v
      Nothing -> error "Interpreter broken"


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


backofficeActions :: [Action]
backofficeActions =
    [ orderService
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


-- | WIP
backofficeText :: Map IBox Text -> Text
backofficeText iMap =
    T.unlines $
    map (fmtAction) backofficeActions
    where
      ctx = TCtx iMap
      indent :: [Text] -> [Text]
      indent = map ("\t" `T.append`)
      fmtAction a =
          T.unlines $
               [lkp (IBox $ aType a) iMap] ++
               (indent $
                [ T.concat ["Время выполнения: ", toText ctx $ due a]
                , "Результаты:" ] ++
                (indent $
                 Prelude.map (\(r, eff) ->
                              T.concat [ lkp (IBox r) iMap
                                       , ": "
                                       , toText ctx eff
                                       ]) $
                 outcomes a)
               )


backofficeGraph :: Map IBox Text -> Gr Text Text
backofficeGraph iMap =
    mkGraph ((finishId, "FINISH"):(map (mkNode) backofficeActions)) $
    concat (map (mkEdges) backofficeActions)
    where
      finishId = 0
      mkEdges :: Action -> [LEdge Text]
      mkEdges a =
          concat $
          map (\(r, o) ->
               toEdge (GraphCtx i finishId r [] (TCtx iMap)) o) $ outcomes a
          where
            Ident i = aType a
      mkNode :: Action -> LNode Text
      mkNode a = (i, lkp (IBox t) iMap)
          where
            t@(Ident i) = aType a


backofficeDot :: Map IBox Text -> LT.Text
backofficeDot iMap =
    printIt $
    graphToDot nonClusteredParams{ fmtNode = \(_, l) -> [toLabel l]
                                 , fmtEdge = \(_, _, l) -> [toLabel l]} $
    backofficeGraph iMap
