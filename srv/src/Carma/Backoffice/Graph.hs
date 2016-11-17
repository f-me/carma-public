module Carma.Backoffice.Graph
    (
      -- * FGL interface
      backofficeNodesEdges
    , backofficeGraph
    , BackofficeGraphData(..)

      -- * GraphViz formatter
    , backofficeDot

      -- * Misc
    , startNode
    , finishNode
    )

where

import           Prelude hiding ((>), (==), (||), (&&), const)
import qualified Prelude as P ((==), (||), const)

import           Control.Monad.Trans.State

import           Data.Graph.Inductive.Graph hiding (toEdge)
import           Data.Graph.Inductive.PatriciaTree
import           Data.GraphViz hiding (fromNode)
import           Data.GraphViz.Printing (printIt)

import           Data.Maybe
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT (Text)

import           Data.Model

import           Carma.Model.ActionResult as ActionResult

import           Carma.Backoffice.DSL
import           Carma.Backoffice.Text


-- | FGL graph edge embedding. A DSL term is converted to a list of
-- edges depending on all possible outcomes. Only terms with semantic
-- type 'ActionOutcome' are interpreted into non-null lists. Chained
-- effects and switch conditions are marked on edge labels with @*@
-- and @?@ symbols.
--
-- A switch condition results in (b+1) extra edges, where b is the
-- amount of switch branches (counting the default branch). An extra
-- node is included between source and target nodes when a switch
-- construct occurs.
--
-- This embedding is not total because a term may not produce any
-- edges. There're several reasons for this.
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
-- processed (to prevent collisions between node numbers).
newtype EdgeE t =
  EdgeE { toEdge :: EdgeCtx -> NodeGenerator [LEdge ColoredLabel] }


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
nothing = EdgeE $ P.const $ return []


instance Backoffice EdgeE where
    now = nothing
    justNow = nothing
    since _ _ = nothing
    before _ _ = nothing

    nobody = nothing
    currentUser = nothing
    assigneeOfLast _ _ _ = nothing

    noResult = nothing
    previousAction = nothing

    userField _ = nothing
    caseField _ = nothing
    serviceField _ = nothing

    onField _ _ body = EdgeE $ \c -> toEdge body c
    insteadOf f v body = onField f v body

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
                    evalEdge c{ edgeText =
                                [T.append switchLabel $ T.pack $ show i]
                              , edgeColor = Just col
                              , fromNode = swNode
                              } br
            in do
              swNode <- mkNewNode
              -- Insert intermediate switch node between source node
              -- and branch destinations
              let toSwitch = (fromNode c, swNode, (fullEdgeText c, Nothing))
              brs <- mapM (branchToEdge swNode) $
                     zip3 branches [(1::Int)..] branchColors
              return $ toSwitch:concat brs

    oneOf _ _ = nothing

    const _ = nothing
    just _ = nothing
    justTxt _ = nothing
    req _ = nothing

    setCaseField _ _ = nothing
    setServiceField _ _ = nothing
    sendMail _ = nothing
    sendSMS _ = nothing

    when _ _ = nothing

    closePrevious _ _ _ = nothing

    defer =
        EdgeE $ \c ->
            return
            [(fromNode c, fromNode c, (fullEdgeText c, edgeColor c))]
    proceed [] =
        EdgeE $ \c ->
            return
            [(fromNode c, finalNode c, (fullEdgeText c, edgeColor c))]
    proceed acts =
        EdgeE $ \c ->
            return $
            map (\(Ident ai) ->
                 (fromNode c, ai, (fullEdgeText c, edgeColor c))) acts

    -- Mark presence of left-hand effects
    _ *> b = EdgeE $ \c -> evalEdge c{edgeText = edgeText c ++ ["*"]} b

    -- FIXME: mark context switching on a graph somehow
    withRelatedService f = f


-- | EdgeE evaluator for DSL terms.
evalEdge :: EdgeCtx -> EdgeE v -> NodeGenerator [LEdge ColoredLabel]
evalEdge ctx (EdgeE f) = f ctx


-- | Internal ActionType-like code for action graph initial state.
-- Edges produced from 'Entry' structures start from this node.
--
-- Used only when a back office graph is analyzed or printed. Actions
-- of this type are never actually created. No ActionType ident must
-- collide with any of these ids (this condition holds for a back
-- office validated with
-- 'Carma.Backoffice.Validation.checkBackoffice').
startNode :: LNode Text
startNode = (-1, "START")


-- | Like 'startNode', but for final state. Edges produced from
-- 'close' terms end at this node.
finishNode :: LNode Text
finishNode = (0, "FINISH")


switchLabel :: Text
switchLabel = "?"


-- | Back office graph parts.
data BackofficeGraphData =
    BGr { _allNodes :: [LNode Text]
        , _allEdges :: [LEdge ColoredLabel]
        , _switchNodes :: [LNode Text]
        -- ^ Switch nodes (also included in 'allNodes').
        }


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
backofficeNodesEdges :: [IdentI ActionResult]
                     -- ^ Ignored action results.
                     -> BackofficeSpec
                     -> Map IBox Text
                     -> BackofficeGraphData
backofficeNodesEdges skipResults spec iMap =
    BGr (stateNodes ++ switchNodes) allEdges switchNodes
    where
      -- First, build graph nodes for states (action types plus
      -- start/finish states)
      stateNodes :: [LNode Text]
      stateNodes = startNode:
                   finishNode:
                   map mkNode (snd spec)
      mkNode :: Action -> LNode Text
      mkNode a = (i, lkp (IBox t) iMap)
          where
            t@(Ident i) = aType a
      -- Find out first untaken node id and use it to produce switch
      -- nodes
      firstSwitchNode :: Node
      firstSwitchNode = 1 + maximum (map fst stateNodes)
      -- Generate all edges. Count and generate extra nodes produced
      -- for switch constructs.
      (allEdges, nextSwitchNode) = runState mkEdges firstSwitchNode
      switchNodes :: [LNode Text]
      switchNodes = zip [firstSwitchNode .. nextSwitchNode - 1] $
                    repeat switchLabel
      mkEdges :: NodeGenerator [LEdge ColoredLabel]
      mkEdges = do
        entries <- mapM mkEntryEdges $ fst spec
        results <- mapM mkResultEdges $ snd spec
        return $ concat $ entries ++ results
      mkEntryEdges :: Entry -> NodeGenerator [LEdge ColoredLabel]
      mkEntryEdges e =
          evalEdge (EdgeCtx
                    (fst startNode)
                    (fst finishNode)
                    ["T"]
                    Nothing) $ trigger e
      mkResultEdges :: Action -> NodeGenerator [LEdge ColoredLabel]
      mkResultEdges a = do
        let Ident i = aType a
        concat <$> mapM
                   (\(r, o) ->
                    evalEdge (EdgeCtx
                              i
                              (fst finishNode)
                              [lkp (IBox r) iMap]
                              Nothing) o)
                   (filter (\o -> fst o `notElem` skipResults) $ outcomes a)


backofficeGraph :: [IdentI ActionResult]
                -> BackofficeSpec -> Map IBox Text -> Gr Text ColoredLabel
backofficeGraph skipResults spec iMap = mkGraph n e
    where (BGr n e _) = backofficeNodesEdges skipResults spec iMap


-- | Produce GraphViz .dot code.
backofficeDot :: [IdentI ActionResult]
              -> BackofficeSpec -> Map IBox Text -> LT.Text
backofficeDot skipResults spec iMap =
    printIt $
    graphToDot nonClusteredParams{ fmtNode = fmtN
                                 , fmtEdge = fmtE} $
    backofficeGraph skipResults spec iMap
    where
      fmtE (_, _, (l, c)) =
          [ toLabel l
          , color $ fromMaybe Black c]
      fmtN n@(_, l) =
          [ toLabel l
          , shape $
            if (n P.== finishNode) P.|| (n P.== startNode)
            then DoubleCircle
            else if l P.== switchLabel
                 then DiamondShape
                 else Ellipse
          ]
