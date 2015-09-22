
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Applicative
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           Text.Read (readMaybe)
import           Text.Regex

import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (parseTime, Day, fromGregorian)
import qualified Data.Configurator as Config
import qualified Data.Aeson as Aeson
import           Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           System.Posix.Syslog
import qualified System.Environment as Env

import           Web.Scotty
import           Network.HTTP.Types.Status (status401)


main :: IO ()
main = do
  prog <- Env.getProgName
  Env.getArgs >>= \case
    [configPath] -> do
      conf <- Config.load [Config.Required configPath]

      logLevel <- fromMaybe (error "Invalid log_level in config")
          . readMaybe <$> Config.require conf "log.level"

      withSyslog prog [PID] USER (logUpTo logLevel) $ do
        syslog Info $ "Loading config from " ++ configPath

        carmaUsr  <- Config.require conf "carma.user"
        carmaPrg  <- Config.require conf "carma.subprogram"
        carmaTest <- Config.require conf "carma.test_mode"
        httpPort  <- Config.require conf "http.port"
        pgHost    <- Config.require conf "pg.host"
        pgPort    <- Config.require conf "pg.port"
        pgUser    <- Config.require conf "pg.user"
        pgPwd     <- Config.require conf "pg.pass"
        pgDb      <- Config.require conf "pg.db"

        syslog Info $ "Connecting to Postgres on " ++ pgHost
        let cInfo = PG.ConnectInfo
              pgHost pgPort
              pgUser pgPwd
              pgDb
        pgPool <- createPool (PG.connect cInfo) PG.close
            1 -- number of distinct sub-pools
              -- time for which an unused resource is kept open
            (fromInteger 20) -- seconds
            5 -- maximum number of resources to keep open

        syslog Info $ "Starting HTTP server on port " ++ show httpPort
        scotty httpPort
          $ httpServer pgPool
          $ SrvConfig carmaPrg carmaUsr carmaTest

    _ -> error $ "Usage: " ++ prog ++ " <config.conf>"


data SrvConfig = SrvConfig
  { cfgSubProgram :: Int
  , cfgCommitter  :: Int
  , cfgTestMode   :: Bool
  }

httpServer :: Pool PG.Connection -> SrvConfig -> ScottyM ()
httpServer pgPool cfg = do
  let queryJSON q = do
        [[res]] <- liftIO $ withResource pgPool $ \c -> PG.query_ c q
        json (res :: Aeson.Value)

  get "/CarMake" $ queryJSON
    [sql| with r as (select id, label from "CarMake")
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]

  get "/CarModel" $ queryJSON
    [sql| with r as (
            select id, label, parent as "carMake"
              from "CarModel"
              where parent = 28
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]

  get "/Dealer" $ queryJSON
    [sql| with r as (
            select id, name, addrs, phones
              from partnertbl p
              where isActive
                and isDealer
                and 28 = any(p.makes) -- make.id = 28 for Mazda
          )
          select array_to_json(array_agg(row_to_json(r.*)))
            from r
    |]


  let handleErrors f = f `rescue` \err -> status status401 >> text err

  post "/Contract/Mazda" $ handleErrors $ do
    vin      <- param "vin"      :: ActionM Text
    carMake  <- param "carMake"  :: ActionM Int
    carModel <- param "carModel" :: ActionM Int
    sellDate <- param "sellDate" :: ActionM Text
    dealer   <- param "dealer"   :: ActionM Int

    let vinRx = mkRegexWithOpts "^[0-9A-HJ-NPR-Z]{17}$" False True
    let vinOk = isJust $ matchRegex vinRx $ T.unpack vin
    when (not vinOk) $ raise "Invalid VIN format."
    when (not True)  $ raise "Invalid VIN checksum."
    when (carMake /= 28) $ raise "Invalid carMake."

    let minDay = fromGregorian 1982 3 15 :: Day
    let maxDay = fromGregorian 2019 3 19 :: Day
    parsedDate <- case parseTime undefined "%F" $ T.unpack sellDate of
      Nothing -> raise "Invalid sellDate format. Should be YYYY-MM-DD."
      Just day
        | minDay < day && day < maxDay -> return day
        | otherwise -> raise "sellDate is out of range."

    [[modelOk, dealerOk]] <- liftIO $ withResource pgPool $ \c -> PG.query c
      [sql| select
        exists (select id from "CarModel" where parent = 28 and id = ?),
        exists (
          select id from partnertbl
          where id = ? and isActive and isDealer and 28 = any(makes))
      |] [carModel, dealer]

    when (not modelOk)  $ raise "Invalid carModel"
    when (not dealerOk) $ raise "Invalid dealer"

    void $ liftIO $ withResource pgPool $ \c -> PG.execute c
      [sql| insert into "Contract"
                ( vin, make, model, seller, subrogram
                , validsince, validuntil
                , dixi, committer)
              with p as (select
                  ? :: text    as vin,
                  ? :: int     as make,
                  ? :: int     as model,
                  ? :: int     as seller,
                  ? :: date    as validsince,
                  ? :: int     as subprogram,
                  ? :: boolean as testmode,
                  ? :: int     as committer)
              select
                  p.vin, p.make, p.model, p.seller, p.subprogram,
                  p.validsince,  p.validsince + interval '1 year',
                  p.testmode, p.committer
                from p
                where not exists (
                  select id from "Contract"
                    where vin = p.vin
                      and model = p.model
                      and seller = p.seller
                      and validsince = p.validsince)
      |]
      ( vin, carMake, carModel, dealer, parsedDate
      , cfgSubProgram cfg, cfgTestMode cfg, cfgCommitter cfg)
    text "Ok"

