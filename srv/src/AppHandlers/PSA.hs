{-|

API methods used by PSA integration points.

-}

module AppHandlers.PSA
    ( psaCasesHandler
    , repTowagesHandler
    )

where

import Data.ByteString.Char8 (readInt)
import qualified Data.Vector as V
import Snap.Snaplet.PostgresqlSimple
import Snap

import Carma.Model.Program as Program

import AppHandlers.PSA.Base
import Application
import Util


-- | Read program name from @program@ request parameter (citroen
-- or peugeot), serve JSON list of case numbers for that program to
-- be exported to SAGAI, as selected by 'psaQuery'. If @program@ is
-- not present, serve list of all exportable case numbers according to
-- 'psaQuery0'.
psaCasesHandler :: AppHandler ()
psaCasesHandler = do
  program <- getParam "program"
  let pids = case program of
               Just "citroen" -> [Program.citroen]
               Just "peugeot" -> [Program.peugeot]
               Just _         -> error "Program must be citroen or peugeot"
               Nothing        -> [Program.citroen, Program.peugeot]
  rows <- query psaQuery (Only $ V.fromList pids)
  writeJSON (map head rows :: [Int])


-- | Handler wrapper for 'repTowages'. Read case id from @id@ request
-- parameter, serve JSON list of service references if the case has a
-- repeated towage.
repTowagesHandler :: AppHandler ()
repTowagesHandler = do
 cid <- fmap readInt <$> getParam "id"
 case cid of
   Just (Just (n, _)) ->
       do
         ids <- repTowages n
         writeJSON ids
   _ -> error "Could not read case id from request"
