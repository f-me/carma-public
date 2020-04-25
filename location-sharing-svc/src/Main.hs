module Main where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Exception (handle, SomeException)
import           Data.Pool (createPool)
import qualified Data.Configurator as Config
import qualified Data.Text.IO as T
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Environment as Env
import           Text.InterpolatedString.QM (qm)

import           Logger (startLogThread, logMsg, Priority(..))
import           Types (AppContext(..))
import           HttpServer (runServer)


main :: IO ()
main
  = handle (\e -> logMsg Error [qm| {e::SomeException} |])
  $ Env.getArgs >>= \case
    [configPath] -> realMain configPath
    _ -> do
      prog <- Env.getProgName
      logMsg Error [qm| Usage: {prog} <config.conf> |]


realMain :: FilePath -> IO ()
realMain configPath = do
  conf  <- Config.load [Config.Required configPath]
  pgUri <- Config.require conf "pg.uri"
  cxt <- AppContext
    <$> pure pgUri
    <*> createPool (PG.connectPostgreSQL pgUri) PG.close
        1   -- number of distinct sub-pools
        200 -- seconds unused resource kept open
        5   -- maximum number of resources to keep open
    <*> Config.require conf "http.port"
    <*> Config.require conf "url.prefix"
    <*> (Config.require conf "html.index" >>= T.readFile)

  startLogThread
  runServer cxt
