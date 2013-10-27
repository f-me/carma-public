{-# LANGUAGE FlexibleInstances #-}
module AppHandlers.GitStats where

import Control.Monad.IO.Class

import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))

import Data.Time.Format
import Data.Time.Clock
import Data.String.Utils
import qualified  Data.Map as M

import System.Locale
import System.Process

import Snap
import Snap.Snaplet.Heist

import qualified Text.Blaze.Html as H
import Text.Blaze.Html (ToMarkup(..))
import qualified Text.Blaze as H
import Text.Blaze.Html5 ((!), html, body, link, toHtml, table, td, tr, ul, li, th, thead)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as H (rel, href, class_, width)
import qualified Text.Blaze.Renderer.Utf8 as H
import Text.Blaze
import GitStats
import Application
import AppHandlers.Util

data Commit  = Commit String String String UTCTime deriving Show

type Release = (String, UTCTime)
type Releases = M.Map String [UTCTime]

parseRelease :: String -> Release
parseRelease s =
  let hsh  = head $ words s
      t    = unwords $ tail $ words s
      time = readTime defaultTimeLocale "%F %X %z" t
  in (hsh, time)

parseCommit :: String -> Commit
parseCommit s =
  let [h, u, c, t] = split "|" s
      time = readTime defaultTimeLocale "%F %X %z" t
  in Commit h u c time

toReleases :: [Release] -> Releases
toReleases rs = foldl (\m (h, t) -> M.insertWith (++) h [t] m) M.empty rs

------------------------------------------------------------------------------
-- | Serve a JSON object with build-time Git information.
serveGitStats :: AppHandler ()
serveGitStats =
    writeJSON $
    Aeson.object [ "gitCommitHash" .= gitCommitHash
                 , "gitCommitTime" .= gitCommitTime
                 ]

serveReleases :: AppHandler ()
serveReleases = do
  commits <- liftIO $ readProcess "git"
             ["log"
             ,"--pretty=format:%h|%an|%s|%ci"
             ,"--since='6 months ago'"
             ,gitCommitHash
             ]
             ""
  releases <- liftIO $ readFile $ "release-" ++ gitBranch
  let commits' = map parseCommit $ lines commits
      releases' = toReleases $ map parseRelease $ lines releases
  writeLBS $ H.renderHtml $ html $ do
    H.head $ do
      link ! rel "stylesheet" ! href "/s/css/bootstrap.min.css"
      link ! rel "stylesheet" ! href "/s/css/datepicker.css"
      link ! rel "stylesheet" ! href "/s/css/jquery.dataTables.css"
    body $ do
      table ! class_ "table table-stripped" $ do
        thead $ tr $ do
          th ! width "5%"  $ "hsh"
          th ! width "5%"  $ "user"
          th ! width "50%" $ "msg"
          th ! width "20%" $ "time"
          th ! width "20%" $ "compile time"
        toHtml $ prepareCommits commits' releases'

prepareCommits :: [Commit] -> Releases -> [(Commit, Maybe [UTCTime])]
prepareCommits (cs@(Commit h _ _ _):xs) rs =
  (cs, M.lookup h rs) : prepareCommits xs rs

instance ToMarkup [(Commit, Maybe [UTCTime])] where
  toMarkup a = mapM_ (\v -> tr $ toHtml v) a

instance ToMarkup (Commit, Maybe [UTCTime]) where
  toMarkup ((Commit h u m t), ts) = do
    td $ toHtml h
    td $ toHtml u
    td $ toHtml m
    td $ toHtml t
    td $ toHtml ts

instance ToMarkup UTCTime where
  toMarkup t = toHtml $ formatTime defaultTimeLocale "%F %X" t

instance ToMarkup [UTCTime] where
  toMarkup t =
    case length t > 5 of
      False -> ul ! class_ "unstyled" $ mapM_ (\t -> li $ toHtml t) t
      True  -> ul ! class_ "unstyled" $ do
        mapM_ (\t -> li $ toHtml t) $ take 5 t
        li $ toHtml ("..." :: String)

instance ToMarkup a => ToMarkup (Maybe a) where
  toMarkup (Just v) = toMarkup v
  toMarkup Nothing  = toMarkup ("" :: String)
