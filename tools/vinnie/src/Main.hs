import Control.Monad
import Data.Functor

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Copy

import System.Console.CmdArgs.Implicit

import Carma.VIN
import Carma.VIN.Base hiding            (program)
import qualified Carma.VIN.Base as Base (program)

programName :: String
programName = "vinnie"

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
                 , Base.program = Nothing
                 , subprogram = Nothing
                 }
                 &= program programName
    in do
      clArgs <- cmdArgs sample
      res <- runImport (throwError (IE "FOO")) clArgs
      case res of
        Left e -> print e
        Right a -> return ()
