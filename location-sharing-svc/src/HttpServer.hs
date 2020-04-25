module HttpServer where

import           Control.Exception (throwIO)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import           Data.Pool (withResource)
import           Data.Scientific (Scientific)
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics
import           Network.HTTP.Media ((//), (/:))
import           Network.HTTP.Types (Status(..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           Text.InterpolatedString.QM (qm, qn)

import           Types (AppContext(..))
import           Logger (logMsg, Priority(..))

data Location = Location
  { lon, lat, accuracy :: Scientific
  }
  deriving (Show, Generic, Aeson.FromJSON)


type API
  =    Capture "urlKey" Text :> Get '[HTML] L.Text
  :<|> Capture "urlKey" Text :> ReqBody '[JSON] Location
    :> Post '[JSON] Aeson.Value


data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML L.Text where
  mimeRender _ = L.encodeUtf8


serveThePage :: AppContext -> Text -> Handler L.Text
serveThePage AppContext{pgPool, indexTpl} urlKey
  = liftIO $ withResource pgPool $ \c -> do
    res <- PG.query c [qn|
      select p.label
      \ from "LocationSharingRequest" r, casetbl c, "Program" p
      \ where r.caseId = c.id
      \   and p.id = c.program
      \   and r.urlKey = ?
      \   and r.validUntil > now()
      \ limit 1
      |] [urlKey]
    case res of
      [programName]:_ -> return
        $ L.replace "#(programName)" programName
        $ L.replace "#(urlKey)" (L.fromStrict urlKey)
        $ L.fromStrict indexTpl
      _ -> throwIO $ err404 { errBody = "Not found!" }


updateLocation :: AppContext -> Text -> Location -> Handler Aeson.Value
updateLocation AppContext{pgPool} urlKey loc@(Location{..})
  = liftIO $ withResource pgPool $ \c -> do
    res <- PG.query c [qn|
      select insert_location_sharing_response(
      \ key := ? :: text,
      \ lon := ?,
      \ lat := ?,
      \ accuracy := ?)
      |] (urlKey, lon, lat, floor accuracy :: Int)
    case res of
      [[True]] -> do
        logMsg Info [qm| Got {loc} for key="{urlKey}" |]
        return $ Aeson.object ["success" Aeson..= True]
      [[False]] -> do
        logMsg Info [qm| Key "{urlKey}" is not valid |]
        return $ Aeson.object ["error" Aeson..= ("Key is not valid" ::Text)]
      _ -> do
        logMsg Warn [qn| Unexpected result from Postgres {res} |]
        throwIO $ err500 { errBody = "Not successful" }


runServer :: AppContext -> IO ()
runServer cxt@(AppContext{httpPort}) = do
  let logger rq Status{..} bodySize = logMsg Info [qm|
        "{Wai.requestMethod rq} {Wai.rawPathInfo rq}{Wai.rawQueryString rq}"
        \ {statusCode} {maybe "-" show bodySize} {Wai.remoteHost rq}
        |]
  let settings
        = Warp.setPort httpPort
        $ Warp.setLogger logger
        $ Warp.setOnException (\_ e -> logMsg Error [qm| {e} |])
        $ Warp.defaultSettings

  logMsg Info [qm| Starting http server on port :{httpPort} |]
  Warp.runSettings settings
    $ serve (Proxy :: Proxy API)
    $ serveThePage cxt :<|> updateLocation cxt
