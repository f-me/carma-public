{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Fetch all partners from Redis, perform geocoding, address
normalization and sync data to Postgres.

-}

module Main

where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Generics

import Numeric

import Data.Aeson
import qualified Data.Aeson.Generic as G

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Network.HTTP

import Data.Text as T (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.ByteString.Lazy.UTF8 as BU (fromString, toString)

import Data.String

import System.Environment

carmaRequest :: Int -> Request_String
carmaRequest n =
    getRequest $
    Prelude.concat [ "http://localhost:8000/"
                   , "all/partner/?limit="
                   , show n
                   , "&fields=id,name,city,addrDeJure,addrDeFacto"
                   ]


nominatimRequest :: String -> Request_String
nominatimRequest q =
    getRequest $
    Prelude.concat [ "http://192.168.10.2/nominatim/search.php?"
                   , "&accept-language=ru-RU,ru&"
                   , "format=json&addressdetails=1&q="
                   , urlEncode q
                   ]

t :: Text
t = "Moskva"


-- | Extract integer n from "foobar:n".
extractId :: B8.ByteString -> Int
extractId pid =
    case (B8.readInt . B8.tail . B8.dropWhile (/= ':')) pid of
      Just (n, _) -> n


-- | Turn (Just "") back into Nothing, preserve otherwise.
nullifyEmpty :: Maybe String -> Maybe String
nullifyEmpty (Just "") = Nothing
nullifyEmpty t         = t



-- | Subset of partner model relevant to the syncing process
data Partner = Partner { pid :: Int
                       , name :: Maybe String
                       , addrCity :: Maybe String
                       , addrDeJure :: Maybe String
                       , addrDeFacto :: Maybe String
                       }
                   deriving Show


instance FromJSON Partner where
    parseJSON (Object v) =
        Partner <$>
              (extractId <$> v .: "id")                <*>
              (nullifyEmpty <$> (v .:? "name"))        <*>
              (nullifyEmpty <$> (v .:? "city"))        <*>
              (nullifyEmpty <$> (v .:? "addrDeJure"))  <*>
              (nullifyEmpty <$> (v .:? "addrDeFactor"))


-- | Nominatim response
data Address = Address { lat   :: Double
                       , lon   :: Double
                       , displayName :: String
                       , state :: Maybe String
                       , city  :: Maybe String
                       , road  :: Maybe String
                       , house :: Maybe String
                       }
             deriving Show


instance FromJSON Address where
    parseJSON (Object v) =
        Address <$>
               -- Nominatim packs lan/lot in strings
              (readM (v .: "lat"))   <*>
              (readM (v .: "lon"))   <*>
              (v .: "display_name")  <*>
              (v .:? "state")        <*>
              (v .:? "city")         <*>
              (v .:? "road")         <*>
              (v .:? "house")
        where readM = liftM read

-- | Read list of objects from response body string
readFeed :: FromJSON a => String -> Maybe [a]
readFeed = decode . B8.pack


printPartnerLn :: Partner -> IO ()
printPartnerLn p = do
  T.putStrLn $
   T.concat $ map T.pack $
        [ show $ pid p
        , ": "
        , fromMaybe "" $ name p
        ]


processPartner :: Partner -> IO Partner
processPartner p = do
  printPartnerLn p
  newCity <- case (addrCity p) of
               Just a -> normalizeAddress a
               Nothing -> do
                     putStrLn "No city"
                     return Nothing
  newFacto <- case (addrDeJure p) of
               Just a -> normalizeAddress a
               Nothing -> do
                     putStrLn "No address"
                     return Nothing
  let p' = p{ addrCity = newCity
            , addrDeFacto = newFacto
            }
  putStrLn "---"
  return p'


-- | Build address from parts.
assembleAddress :: Address -> Maybe String
assembleAddress a = Just $ concat [show $ lat a, ", ", show $ lon a]


-- | Clean address prior to sending to Nominatim
--
-- Bottom of chain is applied first.
cleanAddress :: String -> String
cleanAddress = T.unpack . 
               T.strip .
               (T.replace "." "") . 
               (T.replace "пр-т" "проспект") . 
               (T.replace "г." "") . 
               T.pack


-- | Try to normalize address using Nominatim geocoding: take first
-- element in geocoding results and reassemble address from parts.
normalizeAddress :: String -> IO (Maybe String)
normalizeAddress oldAddr = do
  let midAddr = cleanAddress oldAddr
      req = (nominatimRequest midAddr)
  rsp <- simpleHTTP req
  (ads :: Maybe [Address]) <- readFeed <$> getResponseBody rsp
  b <- getResponseBody rsp
  newAddr <- case ads of
               Just (a:_) -> return $ assembleAddress a
               Just []    -> return $ Nothing
               Nothing    -> do
                              print $ "Failed to parse Nominatim response to: " ++ show (req)
                              return Nothing
                             
  putStrLn $ concat [ "{", oldAddr, "} → {", midAddr, "} → {"
                    , fromMaybe "ø" newAddr, "}"
                    ]
  return newAddr


defaultLimit :: Int
defaultLimit = 5


main = do
  args <- getArgs
  let n = case args of
            [] -> defaultLimit
            (s:_) -> read s

  r <- simpleHTTP (carmaRequest n)
  -- getRequest produces String results (>_<)
  (Just ps :: Maybe [Partner]) <- readFeed <$> getResponseBody r
  newPs <- mapM processPartner ps
  return ()
