-- This module contains data types used in this service.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Carma.NominatimMediator.Types where

import           GHC.Generics

import           Data.Proxy
import           Data.Function (on)
import           Data.Word (Word64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import           Data.Attoparsec.ByteString.Char8 hiding (choice, string)
import           Data.String (fromString)
import           Data.Map (Map)
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable (Hashable)
import           Data.Aeson
import           Data.Time.Clock (UTCTime)
import           Data.Time.Calendar (Day (ModifiedJulianDay), showGregorian)
import           Data.Swagger hiding (Response)
import           Text.InterpolatedString.QM
import           Text.Read (Read (readPrec), lift, choice)
import           Text.ParserCombinators.ReadP (string)

import           Control.Exception.Base
import           Control.Concurrent.MVar (MVar)

import           Web.HttpApiData
import           Servant.Client hiding (Response)

import           Carma.NominatimMediator.Utils
import           Carma.Monad.IORefWithCounter
import           Carma.Monad.LoggerBus.Types (LogMessage)
import           Carma.Utils.Operators


-- Some type wrappers to avoid human-factor mistakes and also to parse stuff


-- Example: `Lang "ru-RU,ru"`
newtype Lang =
        Lang { fromLang :: T.Text }
        deriving (Eq, Show, Read, Ord, Generic, ToJSON, ToParamSchema)

instance FromHttpApiData Lang where
  parseUrlPiece = Right . Lang

instance ToHttpApiData Lang where
  toUrlPiece = fromLang


newtype SearchQuery =
        SearchQuery { fromSearchQuery :: T.Text }
        deriving (Eq, Show, Read, Ord, Generic, ToJSON, ToParamSchema)

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

instance ToParamSchema Coords where
  toParamSchema _ = mempty
    { _paramSchemaType    = SwaggerString
    , _paramSchemaFormat  = Just "coordinates"
    , _paramSchemaPattern = Just [qn| ^
                                      -?[0-9]+(\.[0-9]+)?
                                      ,
                                      -?[0-9]+(\.[0-9]+)?
                                      $ |]
    }


class HasRequestType a where
  requestType :: a -> RequestType

data RequestType
   = Search
   | ReverseSearch
     deriving (Eq, Enum, Bounded, Generic, Hashable)

instance Show RequestType where
  show Search        = "search"
  show ReverseSearch = "reverse-search"

instance Read RequestType where
  readPrec = choice $ lift <$>
    [ Search        <$ string "search"
    , ReverseSearch <$ string "reverse-search"
    ]

instance ToJSON RequestType where
  toJSON = String . fromString . show

instance ToSchema RequestType where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "RequestType") mempty
    { _schemaParamSchema = mempty
        { _paramSchemaType = SwaggerString
        , _paramSchemaEnum =
            Just $ String . fromString . show <$> [Search, ReverseSearch]
        }
    }


-- Representation of a request, used as a key to reach cached response
data RequestParams
   = SearchQueryReq Lang SearchQuery
   | RevSearchQueryReq Lang Coords
     deriving (Eq, Show, Read, Ord)

instance HasRequestType RequestParams where
  requestType (SearchQueryReq _ _)    = Search
  requestType (RevSearchQueryReq _ _) = ReverseSearch

instance ToJSON RequestParams where
  toJSON x@(SearchQueryReq lang query) = object
    [ "type"  .= requestType x
    , "lang"  .= fromLang lang
    , "query" .= fromSearchQuery query
    ]

  toJSON x@(RevSearchQueryReq lang (Coords lon' lat')) = object
    [ "type" .= requestType x
    , "lang" .= fromLang lang
    , "lon"  .= lon'
    , "lat"  .= lat'
    ]

instance ToSchema RequestParams where
  declareNamedSchema _ = do
    typeRef  <- declareSchemaRef (Proxy :: Proxy RequestType)
    langRef  <- declareSchemaRef $ unwrapperToProxy fromLang
    queryRef <- declareSchemaRef $ unwrapperToProxy fromSearchQuery
    lonRef   <- declareSchemaRef (Proxy :: Proxy Longitude)
    latRef   <- declareSchemaRef (Proxy :: Proxy Latitude)

    pure
      $ NamedSchema (Just "RequestParams") mempty
      { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }

      , _schemaDescription = Just
          [qns| Request params, only "type" and "lang" keys will always be
                there but either "query" or "lon" & "lat" would be provided
                depending on "type" of request ("query" for "search" and
                "lon" & "lat" for "reverse-search"). |]

      , _schemaDiscriminator = Just "type"
      , _schemaRequired = ["type", "lang"]
      , _schemaProperties = [("type", typeRef), ("lang", langRef)]

      , _schemaAllOf
          = Just
          [ Inline $ mempty
              { _schemaProperties = [("query", queryRef)]
              , _schemaParamSchema =
                  mempty { _paramSchemaType = SwaggerObject }
              }
          , Inline $ mempty
              { _schemaProperties = [("lon", lonRef), ("lat", latRef)]
              , _schemaParamSchema =
                  mempty { _paramSchemaType = SwaggerObject }
              }
          ]
      }


data Response
   = SearchByQueryResponse'  [SearchByQueryResponse]
   | SearchByCoordsResponse' SearchByCoordsResponse
     deriving (Eq, Show, Read)

instance HasRequestType Response where
  requestType (SearchByQueryResponse' _)  = Search
  requestType (SearchByCoordsResponse' _) = ReverseSearch

-- First section of the tuple here indicates time when response have been added
-- to the cache so it could be garbage collected later.
type ResponsesCache = Map RequestParams (UTCTime, Response)


data DebugCachedQuery
   = DebugCachedQuery
   { request_params :: RequestParams
   , time           :: T.Text
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance HasRequestType DebugCachedQuery where
  requestType x = requestType $ request_params (x :: DebugCachedQuery)

data DebugCachedResponse
   = DebugCachedResponse
   { request_params :: RequestParams
   , time           :: T.Text
   , response_type  :: RequestType
   , response       :: Value
   } deriving (Eq, Show, Generic, ToJSON)

instance HasRequestType DebugCachedResponse where
  requestType x = requestType $ request_params (x :: DebugCachedResponse)

instance ToSchema DebugCachedResponse where
  declareNamedSchema _ = do
    requestParamsRef <- declareSchemaRef $ unwrapperToProxy' request_params
    timeRef          <- declareSchemaRef $ unwrapperToProxy' time
    responseTypeRef  <- declareSchemaRef $ unwrapperToProxy' response_type

    searchByQueryResponseRef <-
      declareSchemaRef (Proxy :: Proxy [SearchByQueryResponse])
    searchByCoordsResponseRef <-
      declareSchemaRef (Proxy :: Proxy SearchByCoordsResponse)

    pure
      $ NamedSchema (Just "DebugCachedResponse") mempty
      { _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
      , _schemaDiscriminator = Just "response_type"

      , _schemaDescription = Just
          [qns| "response" may vary depending on "response_type" value that
                in case depends on "type" of "request_params". |]

      , _schemaRequired =
          [ "request_params"
          , "time"
          , "response_type"
          ]

      , _schemaProperties =
          [ ("request_params", requestParamsRef)
          , ("time",           timeRef)
          , ("response_type",  responseTypeRef)
          ]

      , _schemaAllOf
          = Just
          [ Inline $ mempty
              { _schemaProperties = [("response", searchByQueryResponseRef)]
              , _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
              }
          , Inline $ mempty
              { _schemaProperties = [("response", searchByCoordsResponseRef)]
              , _schemaParamSchema = mempty { _paramSchemaType = SwaggerObject }
              }
          ]
      }

    where -- Explicit type to solve duplicate record field names
          unwrapperToProxy' :: (DebugCachedResponse -> a) -> Proxy a
          unwrapperToProxy' = unwrapperToProxy


data StatisticResolve
   = RequestIsFailed
   | RequestIsSucceeded Bool -- Indicates if response is added to the cache
   | ResponseIsTakenFromCache
     deriving (Show, Read, Eq, Generic, Hashable)

type StatisticsData
   = Map Day (HashMap (RequestType, StatisticResolve) Integer)


data AppContext
   = AppContext
   { -- Counter for IORef here is for cache synchronizer, it checks if cache
     -- have been changed since last check and if it's not it does nothing.
     responsesCache :: IORefWithCounter ResponsesCache

     -- Shared Nominatim community server requires User-Agent to be provided
   , clientUserAgent :: UserAgent

     -- For client requests to Nominatim,
     -- it contains created HTTP `Manager` and `BaseUrl` of Nominatim server.
   , clientEnv :: ClientEnv

     -- Disable cache for reverse search (search by coordinates).
     -- It makes sence when you realize that searching by coordinates is almost
     -- always unique, so response probably never being taken from cache, no
     -- need to waste resourses by storing cache for it since we have thousands
     -- of requests.
   , cacheForRevSearchIsDisabled :: Bool

     -- A bus to send log messages to
   , loggerBus :: MVar LogMessage

     -- A bus to send request monads to
   , requestExecutorBus ::
       MVar ( RequestParams
            , ClientM Response
            , MVar (Either ClientError (StatisticResolve, Response))
            )

     -- Collected statistics data.
     -- Counter for IORef is for statistics synchronizer with file to pass
     -- writing to a file if statistics haven't change since last check.
   , statisticsData :: IORefWithCounter StatisticsData

     -- A bus for notifying about statistic increments of requests
   , statisticsBus :: MVar (UTCTime, RequestType, StatisticResolve)
   }


data SearchByQueryResponse
   = SearchByQueryResponse
   { place_id     :: Word64
   , licence      :: T.Text
   , osm_type     :: T.Text
   , osm_id       :: Word64
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

instance ToSchema SearchByQueryResponse where
  declareNamedSchema =
    genericDeclareNamedSchema defaultSchemaOptions
      { fieldLabelModifier = namerWithReservedKeywords }

instance HasRequestType SearchByQueryResponse where
  requestType _ = Search


data SearchByCoordsResponseAddress
   = SearchByCoordsResponseAddress
   { attraction     :: Maybe T.Text
   , city           :: Maybe T.Text
   , city_district  :: Maybe T.Text
   , country        :: T.Text
   , country_code   :: T.Text
   , county         :: Maybe T.Text
   , house_number   :: Maybe T.Text
   , postcode       :: Maybe T.Text
   , road           :: Maybe T.Text
   , state          :: T.Text
   , state_district :: Maybe T.Text
   , suburb         :: Maybe T.Text
   , town           :: Maybe T.Text
   , village        :: Maybe T.Text
   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, ToSchema)

instance HasRequestType SearchByCoordsResponseAddress where
  requestType _ = ReverseSearch

data SearchByCoordsResponse
   = SearchByCoordsResponse
   { place_id     :: Word64
   , licence      :: T.Text
   , osm_type     :: T.Text
   , osm_id       :: Word64
   , lat          :: T.Text
   , lon          :: T.Text
   , display_name :: T.Text
   , address      :: SearchByCoordsResponseAddress
   , boundingbox  :: [T.Text]
   } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON, ToSchema)

instance HasRequestType SearchByCoordsResponse where
  requestType _ = ReverseSearch


-- Types for statistics routes

-- A standard count of days, with zero being the day 1858-11-17.
-- Useful for simple ordering for example by `curl | jq` commands.
newtype JulianDay = JulianDay Day deriving (Eq, Show)

instance ToJSON JulianDay where
  toJSON (JulianDay (ModifiedJulianDay x)) = Number $ fromInteger x

instance ToSchema JulianDay where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "JulianDay") mempty
    { _schemaParamSchema = mempty { _paramSchemaType = SwaggerInteger } }

-- Human-readable representation of day such as "1858-11-17" (yyyy-mm-dd).
-- ISO 8601.
newtype ISODay = ISODay Day deriving (Eq, Show)

instance ToJSON ISODay where
  toJSON (ISODay day) = String $ fromString $ showGregorian day

instance ToSchema ISODay where
  declareNamedSchema _ = pure
    $ NamedSchema (Just "ISODay") mempty
    { _schemaParamSchema = mempty { _paramSchemaType = SwaggerString } }

data RequestsStatistics
   = RequestsStatistics
   { total_requests                    :: Integer
   , total_failed                      :: Integer
   , total_succeeded                   :: Integer
   , succeeded_taken_from_cache        :: Integer
   , succeeded_real_requests           :: Integer
   , succeeded_real_added_to_cache     :: Integer
   , succeeded_real_not_added_to_cache :: Integer
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Semigroup RequestsStatistics where
  a <> b
    = a
    { total_requests                    = f total_requests a b
    , total_failed                      = f total_failed a b
    , total_succeeded                   = f total_succeeded a b
    , succeeded_taken_from_cache        = f succeeded_taken_from_cache a b
    , succeeded_real_requests           = f succeeded_real_requests a b
    , succeeded_real_added_to_cache     = f succeeded_real_added_to_cache a b
    , succeeded_real_not_added_to_cache =
        f succeeded_real_not_added_to_cache a b
    }
    where f field = (+) `on` field

instance Monoid RequestsStatistics where
  mempty
    = RequestsStatistics
    { total_requests                    = 0
    , total_failed                      = 0
    , total_succeeded                   = 0
    , succeeded_taken_from_cache        = 0
    , succeeded_real_requests           = 0
    , succeeded_real_added_to_cache     = 0
    , succeeded_real_not_added_to_cache = 0
    }

data StatisticsByRequestType
   = StatisticsByRequestType
   { request_type :: Maybe RequestType -- `Nothing` means all request types
   , statistics   :: RequestsStatistics
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)

data StatisticsDay
   = StatisticsDay
   { julian_day      :: JulianDay
   , iso_day         :: ISODay
   , by_request_type :: [StatisticsByRequestType]
   } deriving (Eq, Show, Generic, ToJSON, ToSchema)


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


-- Custom exceptions

data RequestTimeoutException =
     RequestTimeoutException RequestParams
     deriving Show

data UnexpectedResponseResultException =
     UnexpectedResponseResultException Response
     deriving Show

instance Exception RequestTimeoutException
instance Exception UnexpectedResponseResultException
