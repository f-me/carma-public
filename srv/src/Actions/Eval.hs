
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
  { thisId :: InstanceId
  , objectCache
  , updatedObjects
      :: Map InstanceId (Map FieldName ByteString)
  }

type InstanceId = ByteString
type Object = Map FieldName ByteString
type EvalStateMonad b a = StateT EvalContext (Handler b (Redson b)) a


newObject :: ModelName -> Handler b (Redson b) InstanceId
newObject modelName
  = runRedisDB database
    (CRUD.create modelName M.empty [])
  >>= either (error . show) (return . CRUD.instanceKey modelName) 


getPath' :: InstanceId -> ByteString -> EvalStateMonad a ByteString
getPath' objId p = do
  (objId', f) <- evalPath p objId
  getField f <$> getObj objId

getPath :: ByteString -> EvalStateMonad a ByteString
getPath = (gets thisId >>=) . getPath'

evalPath
  :: ByteString -> InstanceId
  -> EvalStateMonad a (InstanceId, FieldName)
evalPath = go . B8.split '.' 
  where
    go [f] o = return (o, f)
    go (f:fs) o = getObj o >>= go fs . getField f


getObj :: InstanceId -> EvalStateMonad a Object
getObj objId = do
  objs <- gets objectCache
  case M.lookup objId objs of
    Just obj -> return obj
    Nothing -> do
      obj <- lift $ readDb objId
      modify $ \cxt@(EvalContext{..}) -> cxt
        { objectCache = M.insert objId obj objectCache
        }
      return obj


-- FIXME: return empty string
getField :: FieldName -> Object -> ByteString
getField field obj
  = fromMaybe ""
--      (error $ "There is no field named " ++ show field
--            ++ " in object " ++ show obj)
  $ M.lookup field obj


setPath :: ByteString -> ByteString -> EvalStateMonad a ()
setPath p val = gets thisId >>= setPath' p val

setPath' :: InstanceId -> ByteString -> ByteString -> EvalStateMonad a ()
setPath' objId p val = do
  (objId', f) <- evalPath p objId
  setObj f val objId'


setObj :: FieldName -> ByteString -> InstanceId -> EvalStateMonad a ()
setObj field val objId =
  modify $ \cxt@(EvalContext{..}) -> cxt
    { updatedObjects = M.union
        (M.singleton objId $ M.singleton field val)
        objectCache
    }


readDb :: InstanceId -> Handler b (Redson b) Object
readDb objId
  = case B8.split ':' objId of
    [modelName, intId]
      -> runRedisDB database
          (CRUD.read modelName intId)
      >>= either (error . (("At readDb " ++ show objId) ++) . show) return
    _ -> error $ "invalid object id " ++ show objId


updateDb :: InstanceId -> Object -> Handler b (Redson b) ()
updateDb objId obj
  = case B8.split ':' objId of
    [modelName, intId] -> do
      mModel <- getModelNamed modelName
      case mModel of
        Nothing -> error $ "invalid id " ++ show objId
        Just model
          -> runRedisDB database
            (CRUD.update modelName intId obj $ indices model)
          >>= either (error.(("At updateDb " ++ show objId)++).show) return
    _ -> error $ "invalid object id " ++ show objId


evalTemplate :: Template -> EvalStateMonad a ByteString
evalTemplate (Template xs) = return $ B8.concat $ map evalTPart xs
  where
    evalTPart (Str s) = T.encodeUtf8 s
    evalTPart (Expr (Var v)) = error "Not implemented"


withEvalContext :: EvalStateMonad b a -> Hook b
withEvalContext f = \v commit -> do
  thisModel <- getModelName
  thisId <- getInstanceId
  let thisFullId = B8.concat [thisModel, ":", thisId]

  cxt' <- execStateT f $ EvalContext thisFullId M.empty M.empty

  -- NB: we have race conditions if two users change same
  -- instance simultaneously. Hope this is impossible due to
  -- business processes constraints.
  let updObjs = updatedObjects cxt'
  mapM_ (uncurry updateDb) $ M.toList updObjs
  return $ fromMaybe M.empty $ M.lookup thisFullId updObjs
