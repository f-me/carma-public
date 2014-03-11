{-| CLI runner for VINNIE. -}

import Database.PostgreSQL.Simple

import System.Console.CmdArgs.Implicit

import Carma.VIN hiding           (program)
import qualified Carma.VIN as VIN (program)


programName :: String
programName = "vinnie"


main :: IO ()
main = do
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
                 }
                 &= program programName
    in do
      clArgs <- cmdArgs sample
      res <- doImport clArgs
      case res of
        Right (ImportResult (total, good, bad)) -> do
            putStrLn $ concat [show total, " total"]
            putStrLn $ concat [show good, " loaded"]
            putStrLn $ concat [show bad,  " errors"]
        Left e -> print e
