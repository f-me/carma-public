{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|
  
  CLI tool used to perform SAGAI export, with logging and FTP
  operation.

-}

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Aeson
import Data.Either
import Data.Functor
import Data.Dict as D
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

import Network.Curl
import Network.HTTP
import System.IO
import System.Console.CmdArgs
import System.Directory
import System.Log as L
import System.Log.Syslog
import System.Locale

import Carma.HTTP
import Carma.SAGAI


-- | Export case and its services. If successfull, return SAGAI entry
-- as a bytestring, a new value of the COMPOS counter and an export
-- log.
exportCase :: Int
           -> Int
           -> Int
           -> Dict
           -> IO (Either ExportError (BS.ByteString, Int, [String]))
exportCase cnt caseNumber cp wazzup = do
  -- Read case with provided number and all of the associated services
  res <- readInstance cp "case" caseNumber

  let refs = M.lookup "services" res
  servs <-
      case refs of 
        Nothing -> return []
        Just refField ->
            forM (readReferences refField) $
                \(m, i) -> do
                  inst <- readInstance cp m i
                  return (m, i, inst)

  fv <- runExport sagaiFullExport cnt (res, servs) cp wazzup
  case fv of
    Left err ->
        return $ Left err
    Right ((entry, ExportState newCnt), eLog) ->
        return $ Right (entry, newCnt, eLog)


-- | Monad which stores value of the COMPOS counter when exporting
-- multiple cases.
type ComposMonad = StateT Int IO


-- | Export several cases, properly maintaining the COMPOS counter.
-- Return final counter value, list of faulty case numbers with
-- errors, and a bytestring with successfully exported entries.
exportManyCases :: Int
                -- ^ Initial value of the COMPOS counter.
                -> [Int]
                -- ^ List of case IDs.
                -> Int
                -- ^ CaRMa port.
                -> Dict
                -- ^ Wazzup dictionary.
                -> IO (Int, [(Int, ExportError)], BS.ByteString)
exportManyCases initialCnt cases cp wazzup =
    let
        -- Runs 'exportCase' in ComposMonad
        exportCaseWithCompos :: Int
                             -> ComposMonad
                                (Either (Int, ExportError) BS.ByteString)
        exportCaseWithCompos caseNumber = do
          cnt <- get
          res <- liftIO $ exportCase cnt caseNumber cp wazzup
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


dictionariesMethod :: String
dictionariesMethod = "cfg/dictionaries"


wazzupName :: String
wazzupName = "Wazzup"


-- | Load Wazzup dictionary from local CaRMa or a file.
loadWazzup :: Either Int FilePath
           -- ^ CaRMa port or local file path.
           -> IO (Maybe Dict)
loadWazzup (Right fp) = loadDict fp
loadWazzup (Left cp)  = do
  rs <- simpleHTTP $ getRequest $ methodURI cp dictionariesMethod
  rsb <- getResponseBody rs
  -- Read server response into Map ByteString Value, since carma-dict
  -- does not support multi-level dictionaries yet
  let dicts = decode' $ BSL.pack $ rsb :: Maybe (M.Map String Value)
  return $ case M.lookup wazzupName <$> dicts of
    Just (Just v) -> decode' $ encode v
    _             -> Nothing


fetchPSACaseNumbers :: Int -> IO (Maybe [Int])
fetchPSACaseNumbers cp = do
  rs <- simpleHTTP $ getRequest $ methodURI cp "psaCases"
  rsb <- getResponseBody rs
  return $ decode' $ BSL.pack rsb


loggingRules :: IO (IO Rules)
loggingRules = constant [ rule root $ use defaultPolitics ]


logInfo :: String -> ReaderT Log IO ()
logInfo = L.log L.Info . T.pack


logError :: String -> ReaderT Log IO ()
logError =  L.log L.Error . T.pack


mainLog :: ReaderT Log IO a -> IO a
mainLog a = do
  l <- newLog loggingRules [syslog_ programName]
  withLog l a


curlOptions :: [CurlOption]
curlOptions = [ CurlUseNetRc NetRcRequired
              , CurlUpload True
              ]


-- | Upload a file to a root directory of a remote FTP server.
upload :: String
       -- ^ FTP host name. Login credentials for this machine must be
       -- present in .netrc file.
       -> FilePath
       -- ^ Name of local file in current directory to be uploaded.
       -> IO CurlCode
upload ftpHost fileName = do
  fh <- openFile fileName ReadMode
  size <- hFileSize fh
  c <- initialize
  forM_ curlOptions (setopt c)
  _ <- setopt c $ CurlURL $ "ftp://" ++ ftpHost ++ "/" ++ fileName
  _ <- setopt c $ CurlInFileSize $ fromInteger size
  _ <- setopt c $ CurlReadFunction $ handleReadFunction fh
  perform c


-- | curl 'ReadFunction' for a given handle. Used to feed uploaded
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
                       , dictPath      :: Maybe FilePath
                       , verbose       :: Bool
                       , ftpHost       :: Maybe String
                       , argCases      :: [Int]
                       }
               deriving (Show, Data, Typeable)


main :: IO ()
main =
    let
        sample = Options
                 { carmaPort = 8000
                   &= name "p"
                   &= help "HTTP port of local CaRMa, defaults to 8000"
                 , composPath = ".compos"
                   &= name "c"
                   &= help "Path to file used to store COMPOS counter value"
                 , dictPath = Nothing
                   &= name "d"
                   &= help "Path to file with Wazzup dictionary"
                 , verbose = False
                   &= name "v"
                 , ftpHost = Nothing
                   &= name "m"
                   &= help "Hostname of FTP to upload the result to"
                 , argCases = def
                   &= args
                 }
                 &= program programName
    in do
      Options{..} <- cmdArgs $ sample
      mainLog $ do
         logInfo "Starting up"
         logInfo $ "CaRMa port: " ++ show carmaPort

         cwd <- liftIO $ getCurrentDirectory
         logInfo $ "Current working directory is " ++ cwd

         logInfo $ "Reading COMPOS value from " ++ composPath
         cnt <- liftIO $ loadCompos composPath
         logInfo $ "COMPOS counter value: " ++ show cnt

         wazzupRes <-
             case dictPath of
               Just fp -> do
                  logInfo $
                          "Loading Wazzup dictionary from file " ++ fp
                  liftIO $ loadWazzup (Right fp)
               Nothing -> do
                  logInfo "Loading Wazzup dictionary from CaRMa"
                  liftIO $ loadWazzup (Left carmaPort)

         -- If any case numbers supplied on command line, use them.
         -- Otherwise, fetch case numbers from local CaRMa.
         cNumRes <-
             case argCases of
               [] -> liftIO $ fetchPSACaseNumbers carmaPort
               l  -> return $ Just l

         case (wazzupRes, cNumRes) of
           (Nothing, _) ->
               logError "Could not load Wazzup dictionary"
           (_, Nothing) ->
               logError "Could not fetch case numbers from CaRMa"
           (Just wazzup, Just caseNumbers) ->
               do
                 logInfo $ "Exporting cases: " ++ show caseNumbers
                 -- Bulk export of selected cases
                 (newCnt, errors, res) <-
                     liftIO $
                     exportManyCases cnt caseNumbers carmaPort wazzup

                 -- Dump errors if there're any
                 when (not $ null errors) $ forM_ errors $
                    \(i, e) -> logError $
                    "Error in case " ++ show i ++ ": " ++ show e

                 -- Report brief export stats
                 let caseCount = length caseNumbers
                 logInfo $ "Successfully exported " ++ 
                             (show $ caseCount - (length errors)) ++ 
                             " out of " ++ (show caseCount) ++
                             " cases"

                 -- Save new COMPOS value
                 logInfo $ "Saving new COMPOS counter value: " ++ show newCnt
                 liftIO $ saveCompos composPath newCnt

                 -- Dump export result
                 case BS.null res of
                   True ->
                     logInfo $ "No successfully exported cases"
                   False -> do
                     resFn <- liftIO $ dumpResult res
                     logInfo $ "Saved result to file " ++ resFn
                     case ftpHost of
                       Nothing -> return ()
                       Just fh ->
                           do
                             logInfo $ "Using FTP at " ++ fh
                             curlRes <- liftIO $ withCurlDo $ upload fh resFn
                             case curlRes of
                               CurlOK -> logInfo "Successfully uploaded to FTP"
                               e      -> logError $
                                         "Error when uploading: " ++ show e
         logInfo "Powering down"
      threadDelay (1000 * 1000)
