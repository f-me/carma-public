{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Post legacy data directly to Redis hashes, cases only.

TODO: Factor out Snap-agnostic instance creation code from Redson and
use it here.

|-}

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy.UTF8 as LBU (toString)
import Data.Enumerator as E hiding (map)
import qualified Data.Enumerator.Binary as EB

import Data.Map as M

import Data.Aeson
import Data.CSV.Enumerator as CSV

import Database.Redis

import System.Console.CmdArgs.Implicit

data Options = Options
    { caseFile :: Maybe FilePath
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
caseAction :: Connection -> ParsedRow MapRow -> Iteratee B.ByteString IO Connection
caseAction h (ParsedRow Nothing) = return h
caseAction h CSV.EOF             = return h
caseAction h (ParsedRow (Just r)) =
    let
      c = M.toList (filterCaseRow r)
      -- -- TODO Find out why rubbish is posted when using HTTP.
      -- body = encode $ filterCaseRow r
      -- post = LBU.toString body
      -- req = postRequestWithBody casePath "application/json" post
    in do
      liftIO $ runRedis h $ do
         Right n <- incr "global:case:id"
         newId <- return $ (BU.fromString . show) n
         _ <- hmset (B.append "case:" newId) c
         _ <- lpush "global:case:timeline" [newId]
         return ()
      return h


main :: IO ()
main =
    let
        sample = Options
                 { caseFile = def &= help "Path to CSV case archive file"
                 }
                 &= program "hmset-poster"
    in do
      Options{..} <- cmdArgs $ sample
      case caseFile of
        Just fname -> do
          rConn <- connect defaultConnectInfo
          res <- foldCSVFile fname defCSVSettings caseAction rConn
          case res of
            Left e -> error "Failed to process case archive"
            Right h -> return ()
        Nothing -> return ()
