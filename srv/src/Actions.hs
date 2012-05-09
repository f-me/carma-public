{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Actions where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Instances () -- instance Functor Either
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB (readFile)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))

import Snap.Snaplet (Handler)
import Snap.Snaplet.RedisDB (runRedisDB)
import Snap.Snaplet.Redson.Internals
import Snap.Snaplet.Redson.Snapless.Metamodel
import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD


data VarPath
  = VarPath (Maybe ModelName) FieldName
  deriving (Eq, Ord)

data Action = Action
  { a'on  :: Map VarPath (Set FieldValue)
  , a'new :: [Map VarPath Template]
  , a'set :: Map VarPath Template
  , a'del :: Bool
  }

instance FromJSON a => FromJSON (Map VarPath a) where
  parseJSON = fmap (M.mapKeys varPath) . parseJSON
    where
      varPath s = case B8.breakSubstring "." s of
        (fld,"") -> VarPath Nothing fld
        (model,fld) -> VarPath (Just model) $ B8.tail fld

instance FromJSON Action where
  parseJSON (Object o) = Action
    <$> o .:  "on"
    <*> o .:? "new" .!= []
    <*> o .:? "set" .!= M.empty
    <*> o .:? "del" .!= False
  parseJSON _ = mzero


newtype Template = Template [TmpPart]
data TmpPart = Str Text | Expr Expr

data Expr
  = Var ByteString
  | TimeOffset Int -- UTCDiff
  | Call ByteString Expr


instance FromJSON Template where
  parseJSON (String s) = Template <$> goS s
    where
      goS "" = pure []
      goS s  = let (x,xs) = T.breakOn "{{" s
               in  (Str x :) <$> goE (stripPrefix "{{" xs)

      goE "" = pure []
      goE s  = let (x,xs) = T.breakOn "}}" s
               in  (:) <$> (Expr <$> expr x)
                       <*> goS (stripPrefix "}}" xs)

      stripPrefix p s = fromMaybe s $ T.stripPrefix p s

      expr s = pure $ Var ""


parseActions :: FilePath -> IO (Either String [Action])
parseActions fName = do
  res <- parse Aeson.json' <$> LB.readFile fName
  return $ case res of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success actions -> Right actions
      Error err -> Left err
    err -> Left $ show err


compileAction :: Action -> HookMap b
compileAction (Action {..})
  = joinHooks
    [ hook2map path
      $ chkFieldVal vals
      $ withEvalContext
--        $ updateSelf a'set
        $ createActions a'new
--        $ if a'del then archive else nop
    | (path,vals) <- M.toList a'on
    ]


hook2map :: VarPath -> Hook b -> HookMap b
hook2map (VarPath m f)
  = M.singleton (fromMaybe "action" m)
  . M.singleton f . (:[])

chkFieldVal :: Set FieldValue -> Hook b -> Hook b
chkFieldVal vals h = \v commit ->
  if S.member v vals
    then h v commit
    else return commit

type EvalContext = Map ModelName (Map FieldName ByteString)
type EvalStateMonad b a = StateT EvalContext (Handler b (Redson b)) a


redisRead m = runRedisDB database . CRUD.read m
redisRead' longId = redisRead modelName intId
  where
    [modelName, intId] = B8.split ':' longId


withEvalContext :: EvalStateMonad b a -> Hook b
withEvalContext f = \v commit -> do
  currentModel <- getModelName
  currentId <- getInstanceId
  Right this <- redisRead currentModel currentId
  let this' = M.union commit this

  cxt <- case currentModel of
    "action" -> do
      Right svc  <- redisRead' $ this M.! "serviceId"
      Right kaze <- redisRead' $ this M.! "caseId"
      return $ M.fromList
        [("action", this'),("service", svc),("case", kaze)]
    _ -> do -- some service: e.g. towage or tech
      Right kaze <- redisRead' $ this M.! "parentId"
      return $ M.fromList
        [("service", this'),("case", kaze)]

  -- TODO: insert cxt [#now,#currentUser,#dict(,)]
  let cxt' = M.union cxt $ M.fromList
        [("this",this')]
  st <- execStateT f cxt'
  -- TODO: update action, service and case in DB
  -- NB: we have race conditions if two users change same
  -- instance simultaneously. Hope this is impossible due to
  -- business processes constraints.
  return $ st M.! "this"


createActions :: [Map VarPath Template] -> EvalStateMonad b ()
createActions a'new = undefined

joinHooks :: [HookMap b] -> HookMap b
joinHooks = M.unionsWith (M.unionWith (++))

compileActions :: FilePath -> IO (Either String (HookMap b))
compileActions fName
  = fmap (joinHooks . map compileAction)
  <$> parseActions fName
