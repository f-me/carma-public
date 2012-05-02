{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map as M

import RESTLoader


main = do
  txt <- lines <$> getContents
  h <- login
  mapM (go h . BU.fromString) txt
  
go h str
  = create h "partner"
  $ M.fromList [("name",str)]
