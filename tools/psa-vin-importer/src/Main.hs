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


copyStart :: Query
copyStart = [sql|COPY psa_vin_import FROM STDIN (DELIMITER ';');|]


transferContracts :: Query
transferContracts = [sql|
INSERT INTO contracttbl
SELECT p.id as carSeller,
       to_timestamp(valid_from, 'DD-MM-YYYY') as warrantyStart,
       to_timestamp(valid_to, 'DD-MM-YYYY') as warrantyEnd,
       vin_number as carVin,
       license_plate_no as carPlateNum,
       first_registration_date as carBuyDate
FROM psa_vin_import, partnertbl p
WHERE dealer_code = p.code;
|]


programName :: String
programName = "sagai-exporter"


data Options = Options { carmaPort :: Int
                       , url       :: Maybe String
                       , programId :: Maybe Int
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
             , useSyslog = False
               &= explicit
               &= name "syslog"
               &= name "l"
               &= help "Use syslog"
             , programId = Nothing
               &= explicit
               &= name "program"
               &= name "o"
               &= help "Program ID used for imported contracts"
             , url = def
               &= args
               &= typ "URL"
             }
             &= program programName
  in do
    Options{..} <- cmdArgs $ sample
    (fromFile:_) <- getArgs
    (tmp, tmpHandle) <- openTempFile tmpDir "psavin.csv"

    -- Basic CSV preprocessing
    runResourceT $ transformCSV arcCSVSettings
                                (sourceFile fromFile)
                                processor
                                (sinkHandle tmpHandle)
    hClose tmpHandle

    -- Postgres bulk import
    conn <- pgConnect
    copy_ conn copyStart
    BL.readFile tmp >>= mapM_ (putCopyData conn) . BL.toChunks
    res <- putCopyEnd conn

    execute_ conn transferContracts

    close conn
    hClose tmpHandle
    print res
