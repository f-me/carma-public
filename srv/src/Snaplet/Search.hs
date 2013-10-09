{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Database.PostgreSQL.Simple

import Util

import Carma.Model.Case

data Search = Search { postgres :: Pool Connection }

routes = [ ( "services", method POST services) ]

-- let withPG f = gets pg_search >>= liftIO . (`withResource` f)

services :: Handler b Search ()
services = do
  let withPG f = gets postgres >>= liftIO . (`withResource` f)
  l <- getParam "limit"
  let l' = fromMaybe 10 $ return . fst =<< B.readInt =<< l
  b <- Util.readJSONfromLBS <$> readRequestBody 4096
  q <- withPG $ \c -> buildCaseSearchQ c l' b
  -- j <- withPG $ \c -> query_ c $ buildCaseSearchQ c l b
  case q of
    Left  v  -> writeBS $ B.pack v
    Right q' -> do
      v :: [[B.ByteString]] <- withPG $ \c -> query_ c $ fromString $ T.unpack q'
      writeBS $ B.concat $ map B.concat v


searchInit :: Pool Connection -> SnapletInit b Search
searchInit conn = makeSnaplet "search" "Search snaplet" Nothing $ do
  addRoutes routes
  return $ Search conn
