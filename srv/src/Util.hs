{-# LANGUAGE DeriveDataTypeable #-}

module Util
  (readJSON
  ,readJSONfromLBS
  ,selectParse
  ,mbreadInt
  ,mbreadDouble
  ,readDouble
  ,lookupNE
  ,selectPrice
  ,printBPrice
  ,getCostField
  ,upCaseName
  ,bToString
  ,stringToB
  , formatTimestamp
  , render
  , projNow
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe

import Control.Exception
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Typeable
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B

import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Time
import Data.Time.Clock.POSIX
import System.Locale (defaultTimeLocale)

import Data.Aeson as Aeson
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto

import Data.Attoparsec.Combinator (many1, choice)
import qualified Data.Attoparsec.ByteString.Char8 as A

import Text.Printf (printf)


data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f

readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"

readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> throw $ FromJSONError src err
    err -> throw $ AttoparsecError src (show err)


--------------------------------------------------------------------------------
-- param parser for select
sParse :: ByteString -> [ByteString]
sParse prm =
  let A.Done _ r = A.feed (A.parse (c) prm) B.empty
  in r
    where
      n = return . B.pack =<< (trim $ many1 $ A.satisfy $ A.notInClass "<>=")
      p = trim $ choice $ map (A.string) ["<=", "<", ">=", ">", "=="]
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
s2p s = error "Invalid argument of s2p"

selectParse :: Map ByteString ByteString -> ByteString -> Bool
selectParse obj prm =
  let [l,p,r] = sParse prm
      p' = s2p p
  in case Map.lookup l obj of
    Nothing -> False
    Just v  -> p' v r

mbreadInt :: B.ByteString -> Maybe Int
mbreadInt s = B.readInt s >>= r
  where  r (i, "") = Just i
         r _       = Nothing

mbreadDouble :: B.ByteString -> Maybe Double
mbreadDouble s =  B.readDouble s >>= r
  where r (i, "") = Just i
        r _       = Nothing

readDouble :: B.ByteString -> Double
readDouble = fromMaybe 0 . mbreadDouble

-- | Like Map.lookup but treat Just "" as Nothing
lookupNE :: Ord k => k -> Map k B.ByteString -> Maybe B.ByteString
lookupNE key obj = Map.lookup key obj >>= lp
  where lp "" = Nothing
        lp v  = return v

calcCost :: Map ByteString ByteString -> Map ByteString ByteString -> Maybe ByteString
calcCost srv opt = getCost srv opt >>= calcCost'
  where calcCost' cost = do
          count <- lookupNE "count" opt >>= mbreadDouble
          cost' <- mbreadDouble cost
          return $ printBPrice $ cost' * count

getCost :: Map ByteString ByteString -> Map ByteString ByteString -> Maybe ByteString
getCost opt srv = getCostField srv >>= flip lookupNE opt

getCostField :: Map ByteString ByteString -> Maybe ByteString
getCostField srv = lookupNE "payType" srv >>= selectPrice

selectPrice :: ByteString -> Maybe ByteString
selectPrice v
          | v == "ruamc"                             = Just "price2"
          | any (== v) ["client", "mixed", "refund"] = Just "price1"
          | otherwise                               = Nothing

printPrice :: Double -> String
printPrice p = printf "%.2f" p
printBPrice :: Double -> ByteString
printBPrice p = B.pack $ printPrice p

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


-- | Format timestamp as "DD/MM/YYYY".
formatTimestamp tm = case B.readInt tm of
  Just (s,"") -> do
    tz <- liftIO getCurrentTimeZone
    return $ T.pack $ formatTime defaultTimeLocale "%d/%m/%Y"
      $ utcToLocalTime tz
      $ posixSecondsToUTCTime $ fromIntegral s
  _ -> return "???"


-- | Get current UNIX timestamp, round and apply a function to it,
-- then format the result as a bytestring.
projNow :: (Show b, Integral a, Integral b) =>
           (a -> b)
        -> IO ByteString
projNow fn =
  (B.pack . show . fn . round . utcTimeToPOSIXSeconds) <$> getCurrentTime
