
module Actions.Parse
  (parseActions
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Instances () -- instance Functor Either

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB (readFile)

import Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))

import Actions.Types


instance FromJSON Action where
  parseJSON (Object o) = Action
    <$> o .:  "on"
    <*> o .:? "new" .!= []
    <*> o .:? "set" .!= M.empty
    <*> o .:? "close" .!= False
  parseJSON _ = mzero


instance FromJSON Template where
  parseJSON (String s) = parseTemplate s


parseTemplate :: (Applicative m, Functor m) => Text -> m Template
parseTemplate s = Template <$> goS s
    where
      goS "" = pure []
      goS s  = let (x,xs) = T.breakOn "{{" s
               in  (Str x :) <$> goE (stripPrefix "{{" xs)

      goE "" = pure []
      goE s  = let (x,xs) = T.breakOn "}}" s
               in  (:) <$> (Expr <$> expr x)
                       <*> goS (stripPrefix "}}" xs)

      stripPrefix p s = fromMaybe s $ T.stripPrefix p s

      expr = pure . Var . T.encodeUtf8 -- "FIXME: not implemented"


parseActions :: FilePath -> IO (Either String [Action])
parseActions fName = do
  res <- parse Aeson.json' <$> LB.readFile fName
  return $ case res of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success actions -> Right actions
      Error err -> Left err
    err -> Left $ show err

