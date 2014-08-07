module Carma.Backoffice.Validation
    (
      checkBackoffice
    , ValidityError
    )

where

import           Data.Graph.Inductive.Query.BFS

import           Data.List
import           Data.Maybe

import           Data.Model

import           Carma.Backoffice.DSL hiding ((==), (||))
import           Carma.Backoffice.Graph
import           Carma.Backoffice.Text


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
checkBackoffice :: BackofficeSpec -> IMap -> [ValidityError]
checkBackoffice spec iMap =
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
      origNodes = map aType $ snd spec
      uniqNodes = nub origNodes
      BGr _ edges' switches = backofficeNodesEdges spec iMap
      graph = backofficeGraph spec iMap
      outs = map OutOfGraphTarget $
             mapMaybe (\(from, to, _) ->
                       if (Ident to `elem` origNodes) ||
                          (to == finishId) ||
                          (to `elem` map fst switches)
                       then Nothing
                       else Just (Ident from, Ident to)) edges'
