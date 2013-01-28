{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Aeson
import Data.Either
import Data.Dict as D
import qualified Data.Map as M
import qualified Data.Text as T

import Network.HTTP
import Control.Concurrent
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.Log as L
import System.Log.Syslog

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
  servs <- forM (readReferences $ res M.! "services")
           (\(m, i) -> do
              inst <- readInstance cp m i
              return (m, i, inst))

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


usage :: String
usage = "Usage: " ++
        "sagai-export <Wazzup.json> <COMPOS counter file> " ++
        "<CaRMa port number> [<case id1> <case id2> ..]"


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
saveCompos fp n = do
  writeFile fp (show n)


fetchPSACaseNumbers :: Int -> IO [Int]
fetchPSACaseNumbers cp = do
  rs <- simpleHTTP $ getRequest $ methodURI cp "psaCases"
  rsb <- getResponseBody rs
  case decode' $ BSL.pack rsb of
    Just d -> return d
    Nothing -> error "Could not read case numbers from CaRMa response"


loggingRules :: IO (IO Rules)
loggingRules = constant [ rule root $ use defaultPolitics ]


mainLog :: ReaderT Log IO a -> IO a
mainLog a = do
  l <- newLog loggingRules [syslog_ "sagai-exporter"]
  withLog l a


main :: IO ()
main =  mainLog $ do
  args <- liftIO $ getArgs

  when (length args < 3) $ liftIO $ putStrLn usage >> exitFailure

  L.log L.Info "Starting up"
  -- Read CaRMa port
  let carmaPort = read $ args !! 2
      dictPath  = args !! 0
  -- Load Wazzup dictionary
  Just wazzup <- liftIO $ loadDict dictPath

  L.log L.Info $ T.pack $ "CaRMa port: " ++ show carmaPort
  -- Load previous COMPOS value
  let composFile = args !! 1
  cnt <- liftIO $ loadCompos composFile

  L.log L.Info $ T.pack $ "COMPOS counter value: " ++ show cnt

  -- If any case numbers supplied on command line, use them.
  -- Otherwise, fetch case numbers from local CaRMa.
  caseNumbers <-
    case length args == 3 of
      True -> liftIO $ fetchPSACaseNumbers carmaPort
      False -> return $ map read $ drop 3 args

  -- Bulk export of selected cases
  L.log L.Info $ T.pack $ "Exporting cases: " ++ show caseNumbers
  (newCnt, errors, res) <-
      liftIO $ exportManyCases cnt caseNumbers carmaPort wazzup

  -- Save new COMPOS value
  liftIO $ saveCompos composFile newCnt

  -- Dump errors if there're any
  when (not $ null errors) $ forM_ errors $ 
           \(i, e) -> L.log L.Error $ T.pack $ 
                      "Error in case " ++ show i ++ ": " ++ show e

  -- Dump export result
  liftIO $ BS.putStr res

  L.log L.Info "Powering down"
  liftIO $ threadDelay (1000 * 1000)
