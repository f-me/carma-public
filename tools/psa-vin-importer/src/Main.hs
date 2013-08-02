{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{-|

CLI tool used to import VIN database files provided by ARC Europe.

-}

import qualified Data.ByteString.Lazy as BL
import Data.ByteString as BS (ByteString, readFile)

import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List as CL hiding (mapM_)

import Data.CSV.Conduit

import Database.PostgreSQL.Simple.Copy
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

import Network.Curl

import System.Environment
import System.Console.CmdArgs
import System.IO
import System.IO.Temp
import System.Log.Simple as L
import System.Log.Simple.Syslog


arcCSVSettings :: CSVSettings
arcCSVSettings = defCSVSettings{csvSep = ';', csvQuoteChar = Nothing}


-- | Expected number of fields in every row.
numFields :: Int
numFields = 22


-- | Index of COUNTRY_FIRST_SOLD field
countryField :: Int
countryField = 12


-- | Cut out broken rows and non-RU rows.
processor :: Monad m => Conduit (Row ByteString) m (Row ByteString)
processor = CL.filter $ \r ->
            ((length r) == numFields) &&
            (r !! countryField == "RU")


tmpDir :: FilePath
tmpDir = "/tmp"


pgConnect :: IO Connection
pgConnect = connect defaultConnectInfo { connectUser = "carma_db_sync"
                                       , connectPassword = "pass"
                                       , connectDatabase = "carma"
                                       }


cacheCreate :: Query
cacheCreate = [sql|
CREATE TEMPORARY TABLE psa_vin_import
( fdds_id                 text,
  dealer_code             text,
  dealer_name             text,
  valid_from              text,
  valid_to                text,
  vin_number              text,
  licence_plate_no        text,
  make                    text,
  model                   text,
  arc_model_code          text,
  first_registration_date text,
  vehicle_type            text,
  country_first_sold      text,
  assistance_type         text,
  reporting_date          text,
  creation_date           text,
  country_current_reg     text,
  mileage                 text,
  deleted                 text,
  deliver_date            text,
  modification_date       text,
  telematic               text
);
|]


copyStart :: Query
copyStart = [sql|COPY psa_vin_import FROM STDIN (DELIMITER ';');|]


transferContracts :: Query
transferContracts = [sql|
INSERT INTO contracttbl
           (carSeller,
            program,
            owner,
            ctime,
            warrantyStart,
            warrantyEnd,
            carVin,
            carPlateNum,
            carBuyDate,
            carMake,
            carModel,
            dixi,
            isActive)
SELECT p.id,
       ?,
       ?,
       now(),
       to_timestamp(valid_from, 'DD-MM-YYYY'),
       to_timestamp(valid_to, 'DD-MM-YYYY'),
       vin_number,
       licence_plate_no,
       to_timestamp(first_registration_date, 'DD-MM-YYYY'),
       m.value,
       l.value,
       't',
       't'
FROM psa_vin_import
LEFT OUTER JOIN partnertbl p ON dealer_code = p.code
LEFT OUTER JOIN "CarMaker" m ON lower(make) = lower(m.label)
LEFT OUTER JOIN "CarModel" l ON lower(model) = lower(l.label);
|]


programName :: String
programName = "sagai-exporter"


data Options = Options { carmaPort :: Int
                       , url       :: String
                       , programId :: Int
                       , ownerId   :: Int
                       , useSyslog :: Bool
                       }
               deriving (Show, Data, Typeable)


main :: IO ()
main =
  let
    sample = Options
             { carmaPort = 8000
               &= name "p"
               &= help "HTTP port of local CaRMa, defaults to 8000"
             , url = def
               &= argPos 0
               &= typ "FILE"
             , programId = def
               &= argPos 1
               &= typ "PROGRAM-ID"
             , ownerId = def
               &= argPos 2
               &= typ "OWNER-ID"
             , useSyslog = False
               &= explicit
               &= name "syslog"
               &= name "l"
               &= help "Use syslog"
             }
             &= program programName
  in do
    Options{..} <- cmdArgs $ sample
    let fromFile = url

    (tmp, tmpHandle) <- openTempFile tmpDir "psavin.csv"

    -- Basic CSV preprocessing
    runResourceT $ transformCSV arcCSVSettings
                                (sourceFile fromFile)
                                processor
                                (sinkHandle tmpHandle)
    hClose tmpHandle

    -- Postgres bulk import
    conn <- pgConnect
    execute_ conn cacheCreate
    copy_ conn copyStart
    BL.readFile tmp >>= mapM_ (putCopyData conn) . BL.toChunks
    res <- putCopyEnd conn

    execute conn transferContracts (programId, ownerId)

    close conn
    hClose tmpHandle
    print res
