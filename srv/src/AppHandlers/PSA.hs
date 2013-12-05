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

import Carma.Model.SubProgram as SubProgram

import AppHandlers.PSA.Base
import AppHandlers.Util
import Application


-- | Read subprogram name from @subprogram@ request parameter (citroen
-- or peugeot), serve JSON list of case numbers for that subprogram to
-- be exported to SAGAI, as selected by 'psaQuery'. If @subprogram@ is
-- not present, serve list of all exportable case numbers according to
-- 'psaQuery0'.
psaCasesHandler :: AppHandler ()
psaCasesHandler = do
  program <- getParam "subprogram"
  let pids = case program of
               Just "citroen" -> [SubProgram.citroen]
               Just "peugeot" -> [SubProgram.peugeot]
               Just _         -> error "Subprogram must be citroen or peugeot"
               Nothing        -> [SubProgram.citroen, SubProgram.peugeot]
  rows <- with db $ query psaQuery (Only $ V.fromList pids)
  writeJSON (map head rows :: [Int])


-- | Handler wrapper for 'repTowages'. Read case id from @id@ request
-- parameter, serve JSON list of service references if the case has a
-- repeated towage.
repTowagesHandler :: AppHandler ()
repTowagesHandler = do
 cid <- (liftM readInt) <$> getParam "id"
 case cid of
   Just (Just (n, _)) ->
       do
         ids <- with db $ repTowages n
         writeJSON ids
   _ -> error "Could not read case id from request"
