{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

CLI tool used to perform SAGAI export, with logging and FTP operation.

-}

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Aeson
import Data.Either
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

import Network.Curl
import Network.HTTP
import System.IO
import System.Console.CmdArgs
import System.Directory
import System.Log.Simple as L
import System.Log.Simple.Syslog
import System.Locale

import Carma.HTTP hiding (carmaPort)
import qualified Carma.HTTP as H (carmaPort)
import Carma.SAGAI


-- | Export case and its services. If successfull, return SAGAI entry
-- as a bytestring, a new value of the COMPOS counter and an export
-- log.
exportCase :: Int
           -- ^ Initial value of the COMPOS counter.
           -> Int
           -- ^ Case ID.
           -> ExportDicts
           -> String
           -- ^ Name of an output character set.
           -> CarmaIO (Either ExportError (BS.ByteString, Int, [String]))
exportCase cnt caseNumber ds encName = do
  -- Read case with provided number and all of the associated services
  res <- readInstance "case" caseNumber

  let refs = HM.lookup "services" res
  servs <-
      case refs of
        Nothing -> return []
        Just refField ->
            forM (readReferences refField) $
                \(m, i) -> do
                  inst <- readInstance m i
                  return (m, i, inst)

  fv <- runExport sagaiFullExport cnt (res, servs) ds encName
  case fv of
    Left err ->
        return $ Left err
    Right ((_, ExportState newCnt entry), eLog) ->
        return $ Right (entry, newCnt, eLog)


-- | Monad which stores value of the COMPOS counter when exporting
-- multiple cases.
type ComposMonad = StateT Int CarmaIO


-- | Mark cases as exported to PSA.
markExported :: [Int]
             -- ^ List of case ids.
             -> CarmaIO ()
markExported caseNumbers = do
  forM_ caseNumbers $
        \i -> updateInstance "case" i psaExported
              where
                psaExported = HM.fromList [("psaExported", "1")]


-- | Export several cases, properly maintaining the COMPOS counter
-- value. Return final counter value, list of faulty case numbers with
-- errors, and a bytestring with successfully exported entries.
exportManyCases :: Int
                -- ^ Initial value of the COMPOS counter.
                -> [Int]
                -- ^ List of case IDs.
                -> ExportDicts
                -> String
                -- ^ Name of an output character set.
                -> CarmaIO (Int, [(Int, ExportError)], BS.ByteString)
exportManyCases initialCnt cases ds encName =
    let
        -- Runs 'exportCase' in ComposMonad
        exportCaseWithCompos :: Int
                             -> ComposMonad
                                (Either (Int, ExportError) BS.ByteString)
        exportCaseWithCompos caseNumber = do
          cnt <- get
          res <- lift $ exportCase cnt caseNumber ds encName
          case res of
            Left err -> return $ Left (caseNumber, err)
            Right (entry, newCnt, _) -> do
                     put newCnt
                     return $ Right entry
    in do
      -- Export all cases
      totalRes <- runStateT (forM cases exportCaseWithCompos) initialCnt
      -- Collect errors and concatenate successful results
      let (errors, sagaiEntries) = partitionEithers $ fst totalRes
          finalCnt = snd totalRes
      return (finalCnt, errors, BS.concat sagaiEntries)


-- | Store export result in a file in current directory and return its
-- name.
dumpResult :: BS.ByteString -> IO FilePath
dumpResult res = do
  ct <- getCurrentTime
  tz <- getCurrentTimeZone
  let lt = utcToLocalTime tz ct
      ft = formatTime defaultTimeLocale "%F_%H-%M-%S" lt
      fn = ft ++ ".txt"
  BS.writeFile fn res
  return fn


-- | Load COMPOS counter value from first line of a file. If the file
-- does not exist, return 0.
loadCompos :: FilePath -> IO Int
loadCompos fp = do
  res <- doesFileExist fp
  case res of
    False -> return 0
    True -> do
      h <- openFile fp ReadMode
      l <- hGetLine h
      hClose h
      return $ read l


-- | Write COMPOS counter value to a file.
saveCompos :: FilePath -> Int -> IO ()
saveCompos fp n = writeFile fp (show n)


programName :: String
programName = "sagai-exporter"


fetchExportDicts :: CarmaIO (Maybe ExportDicts)
fetchExportDicts = do
  w <- readDictionary "Wazzup"
  t <- readDictionary "TechTypes"
  c <- readDictionary "CarClasses"
  r <- readDictionary "Result"
  return $ ExportDicts <$> w <*> t <*> c <*> r


-- | Attempt to fetch a list of cases to be exported from CaRMa, as
-- returned by @/psaCases@.
fetchPSACaseNumbers :: Maybe String
                    -- ^ Filter cases by this program name when set.
                    -> CarmaIO (Maybe [Int])
fetchPSACaseNumbers pn = do
  uri <- methodURI ("psaCases/" ++ fromMaybe "" pn)
  rsb <- liftIO $ (simpleHTTP $ getRequest uri) >>= getResponseBody
  return $ decode' $ BSL.pack rsb


logInfo :: String -> ReaderT Log CarmaIO ()
logInfo s = L.log L.Trace $ T.pack s


logError :: String -> ReaderT Log CarmaIO ()
logError =  L.log L.Error . T.pack


mainLog :: Politics -> Logger -> ReaderT Log CarmaIO a -> CarmaIO a
mainLog policy logL a = do
  l <- liftIO $ newLog (constant [ rule root $ use policy ]) [logL]
  withLog l a


curlOptions :: [CurlOption]
curlOptions = [ CurlUseNetRc NetRcRequired
              , CurlUpload True
              ]


-- | Upload a file to a remote FTP server.
upload :: String
       -- ^ URL of FTP server. Login credentials for this machine must
       -- be present in .netrc file.
       -> FilePath
       -- ^ Name of local file in current directory to be uploaded.
       -> FilePath
       -- ^ Remote file name for the uploaded file, possibly including
       -- a relative path from the server root directory.
       -> IO CurlCode
upload ftpURL fileName remotePath = do
  fh <- openFile fileName ReadMode
  size <- hFileSize fh
  c <- initialize
  forM_ curlOptions (setopt c)
  _ <- setopt c $ CurlURL $ ftpURL ++ "/" ++ remotePath
  _ <- setopt c $ CurlInFileSize $ fromInteger size
  _ <- setopt c $ CurlReadFunction $ handleReadFunction fh
  perform c


-- | Curl 'ReadFunction' for a given handle. Used to feed uploaded
-- data to curl library.
handleReadFunction :: Handle -> ReadFunction
handleReadFunction fh ptr size nmemb _ = do
  actualSize <- hGetBuf fh ptr $ fromInteger . toInteger $ (size * nmemb)
  return $
         if (actualSize > 0)
         then Just $ fromInteger $ toInteger actualSize
         else Nothing


-- | Holds all options passed from command-line.
data Options = Options { carmaPort     :: Int
                       , composPath    :: FilePath
                       , ftpServer     :: Maybe String
                       , remotePath    :: Maybe FilePath
                       , encoding      :: String
                       , caseProgram   :: Maybe String
                       , argCases      :: [Int]
                       , useSyslog     :: Bool
                       }
               deriving (Show, Data, Typeable)


testHelp :: String
testHelp = "Not saving new COMPOS value, not flagging exported cases " ++
           "in test mode"


remotePathHelp :: String
remotePathHelp =
    "Relative file name (including path) used when uploading the result " ++
    "to FTP server. " ++
    "If not set, a generic name in the root directory is used."


main :: IO ()
main =
    let
        sample = Options
                 { carmaPort = 8000
                   &= name "p"
                   &= help "HTTP port of local CaRMa, defaults to 8000"
                 , composPath = ".compos"
                   &= name "c"
                   &= help "Path to a file used to store COMPOS counter value"
                 , ftpServer = Nothing
                   &= name "f"
                   &= help ("URL of an FTP server to upload the result to. " ++
                            "Must include URL scheme prefix.")
                 , caseProgram = Nothing
                   &= explicit
                   &= name "program"
                   &= name "o"
                   &= help ("When case id's are not provided explicitly, " ++
                            "export only cases for the specified program")
                 , remotePath = Nothing
                   &= name "r"
                   &= help remotePathHelp
                 , encoding = "UTF-8"
                   &= name "e"
                   &= help "Output encoding"
                 , useSyslog = False
                   &= explicit
                   &= name "syslog"
                   &= name "l"
                   &= help "Use syslog"
                 , argCases = def
                   &= args
                   &= typ "CASEID .. "
                 }
                 &= verbosity
                 &= program programName
    in do
      Options{..} <- cmdArgs $ sample
      let testMode = isNothing ftpServer

      -- True if -v is set
      vv <- isLoud
      -- True if -q is NOT set
      nq <- isNormal

      -- Choose logging facility (stderr or syslog)
      let logL = if useSyslog
                 then syslog_ programName
                 else logger text consoleErr
          -- Translate -v/-q into simple-log logging policy.
          --
          -- When -v is specified, logInfo's are included in the log.
          --
          -- When -q is specified, logInfo's and logError's are
          -- ignored.
          --
          -- When none of -v/-q options are specified, only logError's
          -- are logged.
          --
          -- -v supercedes -q.
          logPolicy = case (vv, nq) of
                     (True, _)  -> Politics L.Trace L.Trace
                     (_, False) -> Politics L.Fatal L.Fatal
                     _          -> Politics L.Info L.Error
          carmaOpts = defaultCarmaOptions{H.carmaPort = carmaPort}

      runCarma carmaOpts $ mainLog logPolicy logL $ do
         logInfo "Starting up"
         when testMode $ logInfo "No FTP host specified, test mode"
         logInfo $ "CaRMa port: " ++ show carmaPort

         cwd <- liftIO $ getCurrentDirectory
         logInfo $ "Current working directory is " ++ cwd

         logInfo $ "Reading COMPOS value from " ++ composPath
         cnt <- liftIO $ loadCompos composPath
         logInfo $ "COMPOS counter value: " ++ show cnt

         -- Load Wazzup dictionaries from CaRMa.
         dictsRes <- do
             logInfo "Loading dictionaries from CaRMa"
             lift fetchExportDicts

         -- If any case numbers supplied on command line, use them.
         -- Otherwise, fetch case numbers from local CaRMa.
         cNumRes <-
             case argCases of
               [] -> do
                 logInfo $
                     "Fetching case numbers from CaRMa (" ++
                     maybe "all valid programs" (++ " program") caseProgram ++
                     ")"
                 lift $ fetchPSACaseNumbers caseProgram
               l  -> do
                 logInfo $ "Using case numbers specified in the command line"
                 return $ Just l

         case (dictsRes, cNumRes) of
           (Nothing, _) ->
               logError "Could not load dictionaries from CaRMa"
           (_, Nothing) ->
               logError "Could not fetch case numbers from CaRMa"
           (Just dicts, Just caseNumbers) ->
               do

                 logInfo $ "Exporting cases: " ++ show caseNumbers
                 -- Bulk export of selected cases
                 (newCnt, errors, res) <-
                     lift $
                     exportManyCases cnt caseNumbers dicts encoding

                 -- Dump errors if there're any
                 when (not $ null errors) $ forM_ errors $
                    \(i, e) -> logError $
                    "Error when exporting case " ++ show i ++ ": " ++ show e

                 -- Report brief export stats
                 let caseCount = length caseNumbers
                     failedNumbers = map fst errors
                     exportedNumbers =
                         filter (\i -> not $ elem i failedNumbers) caseNumbers

                 when (not $ null exportedNumbers) $
                      logInfo $ "Successfully exported " ++
                                  (show $ caseCount - (length errors)) ++
                                  " out of " ++ (show caseCount) ++
                                  " cases"

                 -- Dump export result
                 case BS.null res of
                   True ->
                     logInfo $ "No successfully exported cases"
                   False -> do
                     case ftpServer of
                       -- testMode
                       Nothing -> do
                         logInfo $ testHelp
                         logInfo $ "Dumping result to stdout"
                         liftIO $ BS.putStr res
                       Just fh -> do
                         -- Backup the result locally.
                         resFn <- liftIO $ dumpResult res
                         -- If remote path is not specified, use generic name.
                         let remPath = case remotePath of
                                         Just rp -> rp
                                         Nothing -> resFn
                         logInfo $ "Saved result to file " ++ resFn
                         logInfo $ "Using FTP at " ++ fh
                         logInfo $ "Uploading to " ++ remPath
                         curlRes <- liftIO $
                                    withCurlDo $ upload fh resFn remPath
                         case curlRes of
                           CurlOK -> do
                             logInfo "Successfully uploaded to FTP"

                             -- Save new COMPOS value
                             logInfo $ "Saving new COMPOS counter value: " ++
                                     show newCnt
                             liftIO $ saveCompos composPath newCnt

                             -- Set psaExported field for exported cases
                             logInfo "Flagging exported cases in CaRMa"
                             lift $ markExported exportedNumbers
                           e -> logError $
                                "Error when uploading: " ++ show e
         logInfo "Powering down"
      -- simple-log workaround (don't rush the main thread and wait
      -- for children)
      threadDelay (1000 * 1000)
