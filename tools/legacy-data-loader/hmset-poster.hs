{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Post legacy data directly to Redis hashes, cases only.

|-}

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy.UTF8 as LBU (toString)
import Data.Enumerator as E hiding (map)
import qualified Data.Enumerator.Binary as EB

import Data.Map as M

import Data.CSV.Enumerator as CSV

import Database.Redis

import System.Console.CmdArgs.Implicit

import Snap.Snaplet.Redson.CRUD
import Snap.Snaplet.Redson.Metamodel

data Options = Options
    { caseFile :: Maybe FilePath
    , index    :: [String]
    }
    deriving (Show, Data, Typeable)


-- | Process case row.
--
-- * omit empty fields
--
-- TODO: Combine dates
filterCaseRow :: MapRow -> MapRow
filterCaseRow row = snd $ M.mapEither 
              (\v -> if v == "" then Left v else Right v)
              row


-- | Post case entries directly to Redis using connection provided as
-- accumulator.
--
-- TODO: Split service / program / client references.
--
-- This is like 'mapIntoHandle', but for non-CSV output data.
caseAction :: ([FieldName], Connection) 
           -> ParsedRow MapRow 
           -> Iteratee B.ByteString IO ([FieldName], Connection)
caseAction h      (ParsedRow Nothing)  = return h
caseAction h       CSV.EOF             = return h
caseAction (i, c) (ParsedRow (Just r)) =
    let
      commit = filterCaseRow r
    in do
      liftIO $ runRedis c $
             create "case" commit i
      return (i, c)


main :: IO ()
main =
    let
        sample = Options
                 { caseFile = def 
                   &= help "Path to CSV case archive file"
                   &= typFile
                 , index = [] &= help "Generate index on the field."
                 }
                 &= program "hmset-poster"
                 &= help "hmset-poster -c journal.csv -i field1 -i field2 ..."
    in do
      Options{..} <- cmdArgs $ sample
      case caseFile of
        Just fname -> do
          rConn <- connect defaultConnectInfo
          res <- foldCSVFile fname defCSVSettings caseAction 
                 (Prelude.map BU.fromString index, rConn)
          case res of
            Left e -> error "Failed to process case archive"
            Right h -> return ()
        Nothing -> error "No file selected"
