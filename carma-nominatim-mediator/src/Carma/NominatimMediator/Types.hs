{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Carma.NominatimMediator.Types where

import           GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import           Data.Attoparsec.ByteString.Char8
import           Data.String (fromString)
import           Data.IORef
import qualified Data.Map as M
import           Data.Aeson
import           Data.Aeson.Types (fieldLabelModifier)
import           Data.Time.Clock (UTCTime)

import           Control.Exception.Base
import           Control.Concurrent.MVar

import           Web.HttpApiData
import           Servant.Client

import           Carma.NominatimMediator.Utils


-- Some type wrappers to avoid human-factor mistakes and also to parse stuff


newtype Lang =
        Lang { fromLang :: T.Text }
        deriving (Eq, Show, Read, Ord, Generic, ToJSON)

instance FromHttpApiData Lang where
  parseUrlPiece = Right . Lang

instance ToHttpApiData Lang where
  toUrlPiece = fromLang


newtype SearchQuery =
        SearchQuery { fromSearchQuery :: T.Text }
        deriving (Eq, Show, Read, Ord, Generic, ToJSON)

instance FromHttpApiData SearchQuery where
  parseUrlPiece = Right . SearchQuery

instance ToHttpApiData SearchQuery where
  toUrlPiece = fromSearchQuery


newtype UserAgent =
        UserAgent { fromUserAgent :: T.Text }
        deriving (Eq, Show)

instance FromHttpApiData UserAgent where
  parseUrlPiece = Right . UserAgent

instance ToHttpApiData UserAgent where
  toUrlPiece = fromUserAgent


type Longitude = Double
type Latitude  = Double

data Coords =
     Coords Longitude Latitude
     deriving (Eq, Show, Read, Ord, Generic, ToJSON)

instance FromHttpApiData Coords where
  parseUrlPiece
    = T.encodeUtf8
    ? parseOnly coordsParser
    ? \case Left  e -> Left  $ T.pack e
            Right x -> Right x

    where -- Parses "52.32,3.45" (no spaces)
          coordsParser :: Parser Coords
          coordsParser = Coords <$> longitude <* char ',' <*> latitude

          longitude = double
          latitude  = double


-- Representation of a request, used as a key to reach cached response
data RequestParams
   = SearchQueryReq Lang SearchQuery
   | RevSearchQueryReq Lang Coords
     deriving (Eq, Show, Read, Ord)

instance ToJSON RequestParams where
  toJSON (SearchQueryReq lang query) = object
    [ "type"  .= String "search"
    , "lang"  .= fromLang lang
    , "query" .= fromSearchQuery query
    ]

  toJSON (RevSearchQueryReq lang (Coords lon' lat')) = object
    [ "type" .= String "reverse-search"
    , "lang" .= fromLang lang
    , "lon"  .= lon'
    , "lat"  .= lat'
    ]

data Response
   = SearchByQueryResponse'  [SearchByQueryResponse]
   | SearchByCoordsResponse' SearchByCoordsResponse
     deriving (Eq, Show, Read)

type ResponsesCache = M.Map RequestParams (UTCTime, Response)


data DebugCachedQuery
   = DebugCachedQuery
   { request_params :: RequestParams
   , time           :: T.Text
   } deriving (Eq, Show, Generic, ToJSON)

data DebugCachedResponse
   = DebugCachedResponse
   { request_params :: RequestParams
   , time           :: T.Text
   , response_type  :: T.Text
   , response       :: Value
   } deriving (Eq, Show, Generic, ToJSON)


data AppContext
   = AppContext
   { responsesCache :: IORef ResponsesCache

     -- Shared Nominatim community server requires User-Agent to be provided
   , clientUserAgent :: UserAgent

     -- For client requests to Nominatim,
     -- it contains created HTTP `Manager` and `BaseUrl` of Nominatim server.
   , clientEnv :: ClientEnv

     -- A bus to send log messages to
   , loggerBus :: MVar LogMessage

     -- A bus to send request monads to
   , requestExecutorBus :: MVar ( RequestParams
                                , ClientM Response
                                , MVar (Either ServantError Response)
                                )
   }


data SearchByQueryResponse
   = SearchByQueryResponse
   { place_id     :: T.Text
   , licence      :: T.Text
   , osm_type     :: T.Text
   , osm_id       :: T.Text
   , boundingbox  :: [T.Text]
   , lat          :: T.Text
   , lon          :: T.Text
   , display_name :: T.Text
   , _class       :: T.Text
   , _type        :: T.Text
   , importance   :: Double
   , icon         :: Maybe T.Text
   } deriving (Generic, Eq, Show, Read)

instance FromJSON SearchByQueryResponse where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = namerWithReservedKeywords }

instance ToJSON SearchByQueryResponse where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = namerWithReservedKeywords }

-- Fixes field constructors such as `class` and `type`
namerWithReservedKeywords :: String -> String
namerWithReservedKeywords ('_' : xs) = xs
namerWithReservedKeywords x = x


data SearchByCoordsResponseAddress
   = SearchByCoordsResponseAddress
   { state        :: T.Text
   , country      :: T.Text
   , country_code :: T.Text
   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data SearchByCoordsResponse
   = SearchByCoordsResponse
   { place_id     :: T.Text
   , licence      :: T.Text
   , osm_type     :: T.Text
   , osm_id       :: T.Text
   , lat          :: T.Text
   , lon          :: T.Text
   , display_name :: T.Text
   , address      :: SearchByCoordsResponseAddress
   , boundingbox  :: [T.Text]
   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)


-- Types for requests to Nominatim

data NominatimAPIFormat = NominatimJSONFormat deriving (Show, Eq)

instance ToHttpApiData NominatimAPIFormat where
  toUrlPiece NominatimJSONFormat = "json"

newtype NominatimLon =
        NominatimLon { fromNominatimLon :: Double }
        deriving (Show, Eq)

instance ToHttpApiData NominatimLon where
  toUrlPiece = fromNominatimLon ? show ? fromString

newtype NominatimLat =
        NominatimLat { fromNominatimLat :: Double }
        deriving (Show, Eq)

instance ToHttpApiData NominatimLat where
  toUrlPiece = fromNominatimLat ? show ? fromString


-- Logger types

data LogMessageType = LogInfo | LogError deriving (Show, Eq)
data LogMessage     = LogMessage LogMessageType T.Text deriving (Show, Eq)


-- Custom exceptions

data RequestTimeoutException =
     RequestTimeoutException RequestParams
     deriving (Show)

data UnexpectedResponseResultException =
     UnexpectedResponseResultException Response
     deriving (Show)

instance Exception RequestTimeoutException
instance Exception UnexpectedResponseResultException
