
module Actions.Eval where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B8

import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap.Snaplet (Handler)
import Snap.Snaplet.RedisDB (runRedisDB)
import Snap.Snaplet.Redson.Internals
import Snap.Snaplet.Redson.Snapless.Metamodel
import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD

import Actions.Types

data EvalContext = EvalContext
  { objects   :: Map ModelName (Map FieldName ByteString)
  , objectIds :: Map ModelName ByteString -- InstanceId?
  , models    :: Map ModelName Model
  }


type EvalStateMonad b a = StateT EvalContext (Handler b (Redson b)) a

evalTemplate :: EvalContext -> Template -> ByteString
evalTemplate cxt (Template xs) = B8.concat $ map evalTPart xs
  where
    evalTPart (Str s) = T.encodeUtf8 s
    evalTPart (Expr (Var v)) = case M.lookup field (objects cxt M.! model) of
      Just val -> val
      Nothing  -> (objects cxt M.! "#") M.! "now"
      where
        [model,field] = B8.split '.' v -- FIXME: can fail

cxtAddObject key longId cxt = do
  let [modelName, intId] = B8.split ':' longId
  Right obj <- redisRead modelName intId
  cxtAddObject' key longId obj cxt

cxtAddObject' key longId obj cxt = 
  return $ cxt 
    { objects = M.insert key obj $ objects cxt
    , objectIds = M.insert key longId $ objectIds cxt
    }
  

redisRead m = runRedisDB database . CRUD.read m
redisUpdate m (EvalContext{..}) = do
  let longId = objectIds M.! m
  let obj    = objects   M.! m
  let [modelName, intId] = B8.split ':' longId
  Right _ <- runRedisDB database
        $ CRUD.update modelName intId obj
        $ indices $ models M.! modelName
  return ()


withEvalContext :: EvalStateMonad b a -> Hook b
withEvalContext f = \v commit -> do
  currentModel <- getModelName
  currentId <- getInstanceId
  let currentFullId = B8.concat [currentModel, ":", currentId]

  Right this <- redisRead currentModel currentId
  let this' = M.union commit this

  ms  <- getModels
  now <- round . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  let emptyContext = EvalContext
        { objects = M.singleton "#" $ M.fromList
            [("now", B8.pack $ show (now :: Int))
            ,("currentUser", "back")]
        , objectIds = M.empty
        , models = ms
        }

  cxt <- case currentModel of
    "action" -> return emptyContext
        >>= cxtAddObject  "service" (this' M.! "serviceId")
        >>= cxtAddObject  "case"    (this' M.! "caseId")
        >>= cxtAddObject' "action"  currentFullId this'
    _ -> return emptyContext -- some service: e.g. towage or tech
        >>= cxtAddObject  "case"    (this' M.! "parentId")
        >>= cxtAddObject' "service" currentFullId this'

  -- TODO: insert cxt [#now,#currentUser,#dict(,)]

  cxt' <- execStateT f cxt

  -- NB: we have race conditions if two users change same
  -- instance simultaneously. Hope this is impossible due to
  -- business processes constraints.
  -- FIXME: update only changed fields
  redisUpdate "case" cxt'
  redisUpdate "service" cxt'

  let thisName = if currentModel == "action" then "action" else "service"
  return $ objects cxt' M.! thisName
