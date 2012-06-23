module Utils.HttpErrors (finishWithError) where

import qualified Data.ByteString.Char8 as B
import Snap.Snaplet
import Snap.Core

finishWithError :: Int -> B.ByteString -> Handler a b c
finishWithError code message = do
  modifyResponse $ setResponseCode code
  writeBS message
  r <- getResponse
  finishWith r
