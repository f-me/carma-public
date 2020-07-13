{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  (
  module Carma.Utils.Snap
    -- * String helpers
  , upCaseName
  , bToString
  , stringToB
  , render

    -- * Time and date
  , projNow

    -- * Postgres helpers
  , (:*) (..)
  , ToRowList (..)
  , sqlFlagPair

    -- * Logging
  , syslogJSON, syslogTxt, Syslog.Priority(..)
  , hushExceptions, logExceptions
  , (Aeson..=)

    -- * Spam
  , newTextMail
  , newHtmlMail
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector

import Control.Monad.IO.Class
import qualified Control.Exception as Ex
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted
import Control.Concurrent (myThreadId)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8  as L8
import qualified Data.ByteString.Char8 as B

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Time
import Data.Time.Clock.POSIX

import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.SqlQQ.Alt

import qualified System.Posix.Syslog as Syslog
import qualified System.Posix.Syslog.Functions as Syslog

import           Foreign.C.Types (CInt)
import           Carma.Utils.Snap


-- | Convert UTF-8 encoded BS to Haskell string.
bToString :: ByteString -> String
bToString = T.unpack . T.decodeUtf8

-- | Inverse of 'bToString'.
stringToB :: String -> ByteString
stringToB = T.encodeUtf8 . T.pack

upCaseName :: Text -> Text
upCaseName = T.unwords . map upCaseWord . T.words
  where
    upCaseWord w = T.concat [T.toUpper $ T.take 1 w, T.toLower $ T.drop 1 w]


-- | Simple templater (extracted from SMS module)
render :: Map Text Text
       -- ^ Context.
       -> Text
       -- ^ Template. Context keys @like_this@ are referenced
       -- @$like_this$@.
       -> Text
render varMap = T.concat . loop
  where
    loop tpl = case T.breakOn "$" tpl of
      (txt, "") -> [txt]
      (txt, tpl') -> case T.breakOn "$" $ T.tail tpl' of
        (expr, "")    -> [txt, evalVar expr]
        (expr, tpl'') -> txt : evalVar expr : loop (T.tail tpl'')

    evalVar v = Map.findWithDefault v v varMap


-- | Get current UNIX timestamp, round and apply a function to it,
-- then format the result as a bytestring.
projNow :: (Int -> Int) -> IO Text
projNow fn =
  (T.pack . show . fn . round . utcTimeToPOSIXSeconds) <$> getCurrentTime


-- | Works almost like '(:.)' for 'ToField' instances. Start with `()`
-- and append as many fields as needed:
--
-- > () :* f1 :* f2 :* f3
--
-- Initial `()` saves the type hassle.
data a :* b = a :* b deriving (Eq, Ord, Show, Read)

infixl 3 :*

instance (ToRow a, ToField b) => ToRow (a :* b) where
    toRow (a :* b) = toRow $ a :. Only b


-- | A list of 'ToRow' values with concatenating behavour of 'ToRow'.
data ToRowList a = ToRowList [a]

instance (ToRow a) => ToRow (ToRowList a) where
    toRow (ToRowList l) = concatMap toRow l


-- | Apply a function to a 'Maybe' value, producing a pair with True
-- if Nothing is provided and False otherwise. Similar to 'maybe'.
--
-- This is handy when used with Postgres 'query' in order to support
-- optional select query conditions which are ignored when Nothing is
-- provided:
--
-- > mval <- getParam "someParam"
-- > query "SELECT * FROM foo WHERE (? AND ? = field);"
-- >       (sqlFlagPair (""::ByteString) id mval)
sqlFlagPair :: b
            -- ^ Default value (must have matching type), ignored in
            -- queries.
            -> (a -> b)
            -- ^ Projection if the parameter is Just.
            -> Maybe a
            -- ^ Parameter value.
            -> (Bool, b)
sqlFlagPair def _ Nothing  = (True,  def)
sqlFlagPair _   f (Just v) = (False, f v)


syslogTxt :: MonadIO m => Syslog.Priority -> String -> String -> m ()
syslogTxt p tag msg = syslogJSON p tag ["msg" .= msg]


syslogJSON :: MonadIO m => Syslog.Priority -> String -> [Aeson.Pair] -> m ()
syslogJSON p tag msg = liftIO $ do
  tid <- myThreadId
  let msg' = ("tid" .= show tid) : msg
  let msgBS = L8.concat -- FIXME: escape '%' in messge
        [L8.pack tag, " ", Aeson.encode $ Aeson.object msg']
  B.useAsCString (L8.toStrict msgBS)
    $ flip (Syslog._syslog (toEnum (fromEnum Syslog.User))
        (toEnum (fromEnum p)))
            (fromIntegral (L8.length msgBS) :: CInt)

hushExceptions :: (MonadIO m, MonadBaseControl IO m) => String -> m () -> m ()
hushExceptions tag act = catch act $ \(e :: Ex.SomeException) ->
  syslogJSON Syslog.Warning tag
    ["msg" .= Aeson.String "hushed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]

logExceptions :: (MonadIO m, MonadBaseControl IO m) => String -> m a -> m a
logExceptions tag act = catch act $ \(e :: Ex.SomeException) -> do
  syslogJSON Syslog.Error tag
    ["msg" .= Aeson.String "rethrowed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]
  throw e



newTextMail
  :: PG.Connection
  -> Text -> [Text] -> [Text] -> Text -> Text -> Text -> [Aeson.Pair]
  -> IO ()
newTextMail pg = newMail pg "text/plain; charset=utf-8"


newHtmlMail
  :: PG.Connection
  -> Text -> [Text] -> [Text] -> Text -> Text -> Text -> [Aeson.Pair]
  -> IO ()
newHtmlMail pg = newMail pg "text/html; charset=utf-8"


newMail
  :: PG.Connection
  -> Text -> Text -> [Text] -> [Text] -> Text -> Text -> Text -> [Aeson.Pair]
  -> IO ()
newMail pg mime from to cc reply subj body why
  = do
    res <- uncurry (PG.query pg)
        [sql|
          insert into "Email"
            ("from", "to", cc, reply, mime, subject, body, status, why)
            values
              ($(from)$, $(Vector.fromList to)$ :: text[]
              ,$(Vector.fromList cc)$ :: text[], $(reply)$
              ,$(mime)$
              ,$(subj)$, $(body)$
              ,'please-send'
              ,$(Aeson.object why)$ ::json
              )
          returning id
        |]

    syslogJSON Syslog.Info "newMail" ["msgId" .= (res::[[Int]]), "why" .= why]
