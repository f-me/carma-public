{-# LANGUAGE FlexibleInstances #-}

module Actions.Compile
  (compileActions
  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Snap.Snaplet.RedisDB (runRedisDB)
import Snap.Snaplet.Redson.Internals
import Snap.Snaplet.Redson.Snapless.Metamodel
import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD

import Actions.Types
import Actions.Parse
import Actions.Eval


compileActions :: FilePath -> IO (Either String (HookMap b))
compileActions fName
  = fmap (joinHooks . map compileAction)
  <$> parseActions fName

joinHooks :: [HookMap b] -> HookMap b
joinHooks = M.unionsWith (M.unionWith (++))


compileAction :: Action -> HookMap b
compileAction (Action {..})
  = joinHooks
    [ hook2map path
      $ chkFieldVal vals
      $ withEvalContext
        $ mapM createAction a'new
          >> mapM (uncurry updateObject) (M.toList a'set)
          >> when a'close closeAction
    | (path,vals) <- M.toList a'on
    ]


-- FIXME: translate 'service' pseudomodel to set of true service models
hook2map :: ByteString -> Hook b -> HookMap b
hook2map p
  = M.singleton model
  . M.singleton field . (:[])
  where
    [model,field] = B8.split '.' p -- FIXME: can fail

chkFieldVal :: Set FieldValue -> Hook b -> Hook b
chkFieldVal vals h = \v commit ->
  if S.member v vals
    then h v commit
    else return commit



createAction :: Map FieldName Template -> EvalStateMonad b ()
createAction actionTemplate = do
  cxt@(EvalContext{..}) <- get

  let action  = M.map (evalTemplate cxt) actionTemplate
  let extraFields = M.fromList $
        [("caseId",    objectIds M.! "case")
        ,("serviceId", objectIds M.! "service")
        ,("ctime",     (objects M.! "#") M.! "now")
        ,("closed",    "false")
        ]
        ++ maybe [] (\u -> [("assignedTo", u)])
           (M.lookup "action" objects >>= M.lookup "assignedTo")
        ++ maybe [] (\u -> [("caseId", u)])
           (M.lookup "action" objects >>= M.lookup "caseId")

  let action' = M.union extraFields action
  -- FIXME: do we need to put updated actions into context?
  Right actionId <- lift
        $ runRedisDB database
        $ CRUD.create "action" action'
        $ indices $ models M.! "action"

  let actionId' = B8.append "action:" actionId
  let caseActions = maybe actionId'
        (\actions -> B8.concat [actions, ",", actionId'])
        $ M.lookup "actions" $ objects M.! "case"
  put $ cxt
    { objects = M.update
        (Just . M.insert "actions" caseActions)
        "case" objects
    }


updateObject :: ByteString -> Template -> EvalStateMonad b ()
updateObject p tmp = do
  let [model, field] = B8.split '.' p
  cxt@(EvalContext{..}) <- get
  put $ cxt
    { objects = M.update
        (Just . M.insert field (evalTemplate cxt tmp))
        model objects
    }

closeAction :: EvalStateMonad b ()
closeAction = do
  cxt@(EvalContext{..}) <- get
  let action = M.insert "closed" "true"
        $ objects M.! "action"
  let actionId = objectIds M.! "action"
  let kaze = M.update
        (\a -> Just . B8.intercalate "," . filter (/=actionId) .  B8.split ',' $ a )
        "actions" $ objects M.! "case"
  put $ cxt
    { objects = M.insert "action" action
        $ M.insert "case" kaze objects
    }
  
