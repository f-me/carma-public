{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Snaplet.Search (Search, searchInit)  where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State

import Data.Maybe
import Data.String (fromString)
import Data.Pool
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B

import qualified Data.Aeson as Aeson

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Database.PostgreSQL.Simple as PG

import Util

import Carma.Model.Case

data Search b = Search
  {postgres :: Pool Connection
  ,_auth    :: Snaplet (AuthManager b)
  }
makeLenses ''Search

type SearchHandler b t = Handler b (Search b) t


routes = [ ( "services", method POST services) ]


withPG f = gets postgres >>= liftIO . (`withResource` f)


services :: HandlerSearch b ()
services = do
  l <- getParam "limit"
  let l' = fromMaybe 10 $ return . fst =<< B.readInt =<< l
  b <- Util.readJSONfromLBS <$> readRequestBody 4096
  q <- withPG $ \c -> buildCaseSearchQ c l' b
  -- j <- withPG $ \c -> query_ c $ buildCaseSearchQ c l b
  case q of
    Left  v  -> writeBS $ B.pack v
    Right q' -> do
      v <- withPG $ \c -> query_ c $ fromString $ T.unpack q'
      writeBS $ B.concat $ map B.concat v


searchInit
  :: Pool Connection -> Snaplet (AuthManager b) -> SnapletInit b (Search b)
searchInit conn sessionMgr = makeSnaplet "search" "Search snaplet" Nothing $ do
  addRoutes routes
  return $ Search conn sessionMgr
