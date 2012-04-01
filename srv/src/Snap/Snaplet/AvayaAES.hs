{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.AvayaAES 
    (Avayaplet
    , runAvaya)

where

import Control.Monad.State

import Data.Lens.Common

import Snap.Snaplet

import Network.Avaya

data Avayaplet = Avayaplet {}


------------------------------------------------------------------------------
-- | Perform Avaya action.
runAvaya :: MonadState app m =>
            Lens app (Snaplet Avayaplet)
         -> Conf
         -> Callback
         -> Avaya a -> IO (Either AvayaException ())
runAvaya snaplet conf cb action = runClient conf cb action


------------------------------------------------------------------------------
-- | Make AvayaAES snaplet.
avayaAESInit :: SnapletInit b Avayaplet
avayaAESInit =
    makeSnaplet "snaplet-avaya-aes" "Avaya AES snaplet." Nothing $ do
      return $ Avayaplet
