{-# LANGUAGE FlexibleInstances #-}

module Actions.Compile
  (compileActions
  ,joinHooks
  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift,liftIO)
import Control.Monad.Trans.State

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap.Snaplet.Redson.Internals
import Snap.Snaplet.Redson.Snapless.Metamodel

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
  let [modelName,_] = B8.split ':' thisId

  o <- lift $ newObject "action"
  case modelName of
    "action" ->
      -- copy some fields from current action to the new one
      let copyFields src tgt
            = mapM_ (\f -> getPath' src f >>= setPath' tgt f)
      in copyFields thisId o ["case", "service", "assignedTo"]
    _ -> do -- get caseId from service
        setPath' o "service" thisId
        getPath "parentId" >>= setPath' o "case"

  now <- liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
  setPath' o "ctime" $ B8.pack $ show (now :: Int)
  setPath' o "closed" "false"
  -- let action  = M.map (evalTemplate cxt) actionTemplate
  -- FIXME: do we need to put updated actions into context?

  getPath' o "case.actions"
    >>= setPath' o "case.actions"
      . B8.intercalate "," . (o:) . B8.split ','


updateObject :: ByteString -> Template -> EvalStateMonad b ()
updateObject p tmp = evalTemplate tmp >>= setPath p


closeAction :: EvalStateMonad b ()
closeAction = do
  tId <- gets thisId
  setPath "closed" "true"
  getPath "case.actions"
    >>= setPath "case.actions"
      . B8.intercalate "," . filter (/=tId) . B8.split ','
