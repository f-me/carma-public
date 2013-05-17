{-|

  API methods used by PSA integration points.

-}

module AppHandlers.PSA
    ( psaCasesHandler
    , repTowagesHandler
    )

where

import Data.ByteString.Char8 (readInt)
import Snap.Snaplet.PostgresqlSimple
import Snap

import AppHandlers.PSA.Base
import AppHandlers.Util
import Application


-- | Read program name from @program@ request parameter, serve JSON
-- list of case numbers for that program to be exported to SAGAI, as
-- selected by 'psaQuery'. If @program@ is not present, serve list of
-- all exportable case numbers according to 'psaQuery0'.
psaCasesHandler :: AppHandler ()
psaCasesHandler = do
  program <- getParam "program"
  rows <- with db $ case program of
            Just p -> query psaQuery [p]
            Nothing -> query_ psaQuery0
  writeJSON (map head rows :: [Int])


-- | Handler wrapper for 'repTowages'. Read case id from @id@ request
-- parameter, serve JSON list of service ids for repeated towages.
repTowagesHandler :: AppHandler ()
repTowagesHandler = do
 cid <- (liftM readInt) <$> getParam "id"
 case cid of
   Just (Just (n, _)) ->
       do
         ids <- with db $ repTowages n
         writeJSON ids
   _ -> error "Could not read case id from request"
