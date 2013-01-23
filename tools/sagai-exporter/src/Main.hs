{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString as BS

import Data.Either
import Data.Functor
import Data.Dict as D
import qualified Data.Map as M

import System.IO
import System.Directory
import System.Environment
import System.Exit

import Carma.HTTP
import Carma.SAGAI


-- | Export case and its services. If successfull, return SAGAI entry
-- as a bytestring and a new value of the COMPOS counter.
exportCase :: Int
           -> Int
           -> Int
           -> Dict
           -> IO (Either ExportError (BS.ByteString, Int))
exportCase cnt caseNumber cp wazzup = do
  -- Read case with provided number and all of the associated services
  res <- readInstance cp "case" caseNumber
  servs <- forM (readReferences $ res M.! "services")
           (\(m, i) -> do
              inst <- readInstance cp m i
              return (m, i, inst))

  fv <- runExport sagaiFullExport cnt (res, servs) cp wazzup
  case fv of
    Left err -> return $ Left err
    Right (entry, ExportState cnt) -> return $ Right (entry, cnt)


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
            Right (entry, newCnt) -> do
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
        "sagai-export <Wazzup.json> <COMPOS counter file> <CaRMa port number> " ++
        "<case id1> [<case id2> ..]"


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


main = do
  args <- getArgs
  when (length args < 4) $ putStrLn usage >> exitFailure
  let (carmaPort:caseNumbers) = map read $ drop 2 args
  Just wazzup <- loadDict (args !! 0)

  let composFile = args !! 1
  cnt <- loadCompos composFile

  (newCnt, errors, res) <- exportManyCases cnt caseNumbers carmaPort wazzup

  -- Save new COMPOS value
  saveCompos composFile newCnt

  -- Dump errors if there're any
  when (not $ null errors) $ print errors

  -- Dump export result
  BS.putStr res
