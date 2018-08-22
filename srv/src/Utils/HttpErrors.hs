module Utils.HttpErrors
     ( finishWithError
     ) where

import qualified Data.Text as T
import Snap.Snaplet
import Snap.Core


finishWithError :: Int -> String -> Handler a b c
finishWithError code message = do
  modifyResponse $ setResponseCode code
  writeText $ T.pack message
  getResponse >>= finishWith
