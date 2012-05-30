
module CustomLogic
  (mkFieldHook
  ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB

import Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))

import Snap.Snaplet.Redson.Internals
import Snap.Snaplet.Redson.Snapless.Metamodel



mkFieldHook :: ModelName -> FieldName -> FilePath
            -> IO (Either String (HookMap b))
mkFieldHook model field fName = do
  res <- parseMap fName
  return $ (M.singleton model . M.singleton field . (:[]) . mkH)
        <$> res


mkH :: Map FieldValue Commit -> Hook b
mkH m = \v commit ->
  return $ maybe commit (`M.union` commit)
         $ M.lookup v m


-- FIXME: copypaste from Actions/Parse.hs
parseMap
  :: FilePath
  -> IO (Either String (Map FieldValue Commit))
parseMap fName = do
  res <- parse Aeson.json' <$> LB.readFile fName
  return $ case res of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success actions -> Right actions
      Error err -> Left err
    err -> Left $ show err
