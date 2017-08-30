{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-| CLI runner for VINNIE. -}

import Prelude hiding (takeWhile)

import Control.Exception
import Database.PostgreSQL.Simple

import Data.Attoparsec.Text hiding (Parser)
import Data.Optional
import Control.Applicative as AP
import Data.Text (unpack)
import Data.Version (showVersion)
import Development.GitRev
import Filesystem.Path.CurrentOS
import Paths_vinnie

import System.Exit
import Turtle hiding (decimal, e, skip)

import Carma.VIN


programName :: String
programName = "vinnie"


textToConnInfo :: Text -> Maybe ConnectInfo
textToConnInfo inp =
  let
    notComma = (/= ',')
    comma = (== ',')
    res = flip parseOnly inp $ ConnectInfo
      <$> (unpack <$> takeWhile notComma <* skip comma)
      <*> (decimal <* skip comma)
      <*> (unpack <$> takeWhile notComma <* skip comma)
      <*> (unpack <$> takeWhile notComma <* skip comma)
      <*> (unpack <$> takeWhile notComma)
  in
    case res of
      Right r -> Just r
      _       -> Nothing

main :: IO ()
main =
  let
    desc = fromString $ programName <> " " <> showVersion version <> " " <> $(gitHash)
    optParser :: Parser (ConnectInfo, Options)
    optParser =
      (,)
      <$> opt textToConnInfo "connection" 'c'
           "PostgreSQL connection info (HOST,PORT,USER,PW,DBNAME)"
      <*> (Options
           <$> (encodeString <$> argPath "infile"
                "Input CSV file to import")
           <*> (encodeString <$> argPath "outfile"
                "Location to write CSV report file to")
           <*> argInt "committer-id" Default
           <*> argInt "format-id" Default
           <*> AP.optional (optInt "program" 'p' Default)
           <*> AP.optional (optInt "subprogram" 's' Default)
           <*> switch "arc" 'a' "Set ARC flag when importing contracts")
  in do
      (cInfo, opts) <- options desc optParser
      res <- bracket (connect cInfo) close $ \c -> doImport opts c
      case res of
        Right (ImportResult (total, good, bad)) -> do
            putStrLn $ show total ++ " total"
            putStrLn $ show good ++ " loaded"
            putStrLn $ show bad ++ " errors"
        Left e -> do
            putStrLn $ "Critical error: " ++ show e
            exitFailure
