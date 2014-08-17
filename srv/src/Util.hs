{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  (
    -- * Request processing
    readJSON
  , readJSONfromLBS
  , selectParse
  , mbreadInt
  , mbreadDouble
  , readDouble

   -- * String helpers
  , upCaseName
  , bToString
  , stringToB
  , render

    -- * Time and date
  , formatTimestamp
  , projNow

    -- * Legacy interoperability for Idents
  , identFv
  , fvIdent

    -- * Postgres helpers
  , PlainText(..)
  , (:*)(..)
  , ToRowList(..)
  , sqlFlagPair
  , withPG

    -- * Logging
  , syslogJSON, syslogTxt, Syslog.Priority(..)
  , hushExceptions, logExceptions
  , (Aeson..=)
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Control.Applicative
import Control.Monad.IO.Class
import qualified Control.Exception as Ex
import Control.Monad.CatchIO as IOEx
import Control.Concurrent (myThreadId)
import qualified Blaze.ByteString.Builder.Char8 as BZ
import Data.Typeable
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.Char8  as L8
import qualified Data.ByteString.Char8 as B

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto

import Data.Attoparsec.Combinator (many1, choice)
import qualified Data.Attoparsec.Text as A

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types

import Snap.Snaplet.PostgresqlSimple (Postgres(..), HasPostgres(..))
import qualified Database.PostgreSQL.Simple as P

import qualified Data.Model as Model
import qualified System.Posix.Syslog as Syslog

import Data.Pool

data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Ex.Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f

readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"

readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> Ex.throw $ FromJSONError src err
    err -> Ex.throw $ AttoparsecError src (show err)


--------------------------------------------------------------------------------
-- param parser for select
sParse :: Text -> [Text]
sParse prm =
  let A.Done _ r = A.feed (A.parse c prm) T.empty
  in r
    where
      n = return . T.pack =<< trim (many1 $ A.satisfy $ A.notInClass "<>=")
      p = trim $ choice $ map A.string ["<=", "<", ">=", ">", "=="]
      c = do
        v    <- n
        pred' <- p
        l    <- n
        return [v, pred', l]
      trim pars = A.skipSpace *> pars <* A.skipSpace

s2p :: (Eq s, IsString s, Ord a) => s -> (a -> a -> Bool)
s2p "<=" = (<=)
s2p "<"  = (<)
s2p ">"  = (>)
s2p ">=" = (>=)
s2p "==" = (==)
s2p _    = error "Invalid argument of s2p"

selectParse :: Map Text Text -> Text -> Bool
selectParse obj prm =
  let [l,p,r] = sParse prm
      p' = s2p p
  in case Map.lookup l obj of
    Nothing -> False
    Just v  -> p' v r

mbreadInt :: Text -> Maybe Int
mbreadInt s = case T.decimal s of
  Right (i, "") -> Just i
  _             -> Nothing

mbreadDouble :: Text -> Maybe Double
mbreadDouble s =  case T.double s of
  Right (i,"") -> Just i
  _            -> Nothing

readDouble :: Text -> Double
readDouble = fromMaybe 0 . mbreadDouble


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


-- | Format timestamp as @DD/MM/YYYY@.
formatTimestamp :: MonadIO m => Text -> m Text
formatTimestamp tm = case T.decimal tm of
  Right (s :: Int, "") -> do
    tz <- liftIO getCurrentTimeZone
    return $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y"
      $ utcToLocalTime tz
      $ posixSecondsToUTCTime $ fromIntegral s
  _ -> return "???"


-- | Get current UNIX timestamp, round and apply a function to it,
-- then format the result as a bytestring.
projNow :: (Int -> Int) -> IO Text
projNow fn =
  (T.pack . show . fn . round . utcTimeToPOSIXSeconds) <$> getCurrentTime


-- | Convert an Ident to an untyped field value.
identFv :: Model.Model m => Model.IdentI m -> Text
identFv (Model.Ident v) = T.pack $ show v


-- | Convert an untyped field value to an Ident if it's a numeric
-- string.
fvIdent :: Model.Model m => Text -> Maybe (Model.IdentI m)
fvIdent s = case T.decimal s of
  Right (i,_) -> Just $ Model.Ident i
  _           -> Nothing


-- | Text wrapper with a non-quoting 'ToField' instance.
--
-- Copied from vinnie.
newtype PlainText = PT Text

instance ToField PlainText where
    toField (PT i) = Plain $ BZ.fromText i


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

withPG :: (HasPostgres m)
       => (P.Connection -> IO b) -> m b
withPG f = do
    s <- getPostgresState
    let pool = pgPool s
    liftIO $ withResource pool f


syslogTxt :: MonadIO m => Syslog.Priority -> String -> String -> m ()
syslogTxt p tag msg = syslogJSON p tag ["msg" .= msg]


syslogJSON :: MonadIO m => Syslog.Priority -> String -> [Aeson.Pair] -> m ()
syslogJSON p tag msg = liftIO $ do
  tid <- myThreadId
  let msg' = ("tid" .= show tid) : msg
  let msgBS = L8.concat -- FIXME: escape '%' in messge
        [L8.pack tag, " ", Aeson.encode $ Aeson.object msg']
  B.useAsCString (L8.toStrict msgBS)
    $ Syslog._syslog (toEnum (fromEnum p))

hushExceptions :: MonadCatchIO m => String -> m () -> m ()
hushExceptions tag act = IOEx.catch act $ \(e :: Ex.SomeException) ->
  syslogJSON Syslog.Warning tag
    ["msg" .= Aeson.String "hushed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]

logExceptions :: MonadCatchIO m => String -> m a -> m a
logExceptions tag act = IOEx.catch act $ \(e :: Ex.SomeException) -> do
  syslogJSON Syslog.Error tag
    ["msg" .= Aeson.String "rethrowed exception"
    ,"exn" .= Aeson.String (T.pack $ show e)
    ]
  IOEx.throw e
