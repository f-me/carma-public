{-| CLI runner for VINNIE. -}

import Database.PostgreSQL.Simple

import Data.Version (showVersion)
import Paths_vinnie

import System.Console.CmdArgs.Implicit
import System.Exit

import Carma.VIN hiding           (program)
import qualified Carma.VIN as VIN (program)


programName :: String
programName = "vinnie"


main :: IO ()
main =
  let
        sample = Options
                 { cInfo = defaultConnectInfo
                   &= name "c"
                   &= help "PostgreSQL connection info"
                   &= typ "HOST,PORT,USER,PW,DBNAME"
                 , infile = def
                   &= argPos 0
                   &= typ "IN-FILE"
                 , outfile = def
                   &= argPos 1
                   &= typ "OUT-FILE"
                 , committer = def
                   &= argPos 2
                   &= typ "COMMITTER-ID"
                 , format = def
                   &= argPos 3
                   &= typ "FORMAT-ID"
                 , VIN.program = Nothing
                 , subprogram = Nothing
                 , fromArc = False
                   &= explicit
                   &= name "arc"
                   &= help "Set the flag indicating ARC is the source"
                 }
                 &= verbosity &= verbosityArgs [] [ignore]
                 &= program programName
                 &= summary (programName ++ " " ++ showVersion version)
    in do
      clArgs <- cmdArgs sample
      res <- doImport clArgs
      case res of
        Right (ImportResult (total, good, bad)) -> do
            putStrLn $ show total ++ " total"
            putStrLn $ show good ++ " loaded"
            putStrLn $ show bad ++ " errors"
        Left e -> do
            putStrLn $ "Critical error: " ++ show e
            exitFailure
