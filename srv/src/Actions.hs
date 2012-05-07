{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Actions where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Instances () -- instance Functor Either
import Control.Monad.IO.Class (liftIO)

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
import Snap.Snaplet.Redson
import Snap.Snaplet.Redson.Snapless.Metamodel
import qualified Snap.Snaplet.Redson.Snapless.CRUD as CRUD


data VarPath
  = VarPath (Maybe ModelName) FieldName
  deriving (Eq, Ord)

data Action = Action
  { a'on, a'if :: Map VarPath (Set FieldValue)
  , a'new      :: [Map VarPath Template]
  , a'set      :: Map VarPath Template
  , a'del      :: Bool
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
    <*> o .:? "if"  .!= M.empty
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



type Hook b = FieldValue -> Commit -> Handler b (Redson b) Commit
type HookMap b = M.Map ModelName (M.Map FieldName [Hook b])


compileAction :: Action -> HookMap b
compileAction (Action {..})
  = joinHooks
    [ hook2map path
      $ chkFieldVal vals
      $ chkConstraints a'if
      $ updateSelf a'set
      $ createNew a'new
      $ if a'del then archive else nop
    | (path,vals) <- M.toList a'on
    ]


hook2map :: VarPath -> Hook b -> HookMap b
hook2map (VarPath m f)
  = M.singleton (fromMaybe "action" m)
  . M.singleton f . (:[])

chkFieldVal :: Set FieldValue -> Hook b -> Hook b
chkFieldVal vals h = \v commit ->
  if S.member v vals
    then (liftIO $ print ">>>>> action fired") >> h v commit
    else return commit

chkConstraints a'if k = nop
updateSelf a'set k = nop
createNew a'new k = nop
archive = nop

nop :: Hook b
nop = const return

joinHooks :: [HookMap b] -> HookMap b
joinHooks = M.unionsWith (M.unionWith (++))

compileActions :: FilePath -> IO (Either String (HookMap b))
compileActions fName
  = fmap (joinHooks . map compileAction)
  <$> parseActions fName
