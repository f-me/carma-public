
module AppHandlers.LocationSharing
    ( requestLocation
    )

where

import           Application (AppHandler)
import           AppHandlers.Util (getIntParam, writeJSON)
import qualified Data.Aeson as Aeson
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Snap.Snaplet.PostgresqlSimple (query)
import           Snaplet.Auth.PGUsers (currentUserMetaId)
import           Utils.HttpErrors (finishWithError)

requestLocation :: AppHandler ()
requestLocation = do
  Just uid <- currentUserMetaId
  getIntParam "caseId" >>= \case
    Nothing -> finishWithError 400 "Parameter `caseId` is not valid."
    Just caseId -> do
      [[res]] <- query
        [sql| SELECT row_to_json(create_location_sharing_request(?, ?)) |]
        (caseId :: Int, uid)
      writeJSON (res :: Aeson.Value)
