{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving
           , ScopedTypeVariables
           , RankNTypes
           , DeriveGeneric
           , GADTs
 #-}

module Carma.Model.Types ( Dict(..)
                         , Interval(..)
                         , HMDiffTime(..)
                         , TInt
                         , IdentList
                         , EventType(..)
                         , UserStateVal(..)
                         , Coords
                         ) where

import Control.Applicative ((<$>), (<*>), (*>))
import Control.Monad (void)

import Data.Maybe
import Data.Aeson as Aeson
import Data.String
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Read     as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16

import Data.Int (Int16, Int32)

import qualified Data.Serialize as S
import           Data.Serialize ( Serialize
                                , skip
                                , getFloat64le
                                , putWord8
                                , putWord32le
                                , putFloat64le)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Map as Map

import Data.Either (rights)
import Data.Time
import System.Locale (defaultTimeLocale)

import Text.Read (readMaybe)
import Text.Printf
import Text.Parsec

import Database.PostgreSQL.Simple.FromField (FromField(..)
                                            ,ResultError(..)
                                            ,typename
                                            ,returnError
                                            ,typeOid)
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..), inQuotes)
import Database.PostgreSQL.Simple.TypeInfo.Static (interval, typoid)

import qualified Blaze.ByteString.Builder.Char8 as Builder

import Data.Singletons

import GHC.Generics
import Data.Typeable

import Data.Model
import Data.Model.Types
import Carma.Model.LegacyTypes


-- ISO 8601
instance FromJSON Day where
  parseJSON (String s)
    = case parseTime defaultTimeLocale "%Y-%m-%d" $ T.unpack s of
      Just day -> return day
      Nothing  -> fail $ "invalid date format: " ++ show s
  parseJSON v = fail $ "invalid date: " ++ show v

-- ISO 8601
instance ToJSON Day where
  toJSON = String . fromString . show

newtype Dict m = Dict Text
   deriving (FromField, ToField,
             FromJSON, ToJSON,
             Typeable, IsString)

data Interval t = Interval t t deriving Typeable

instance (Typeable t, Show t) => Show (Interval t) where
  show (Interval begin end) = show begin ++ " | " ++ show end

instance (FromJSON t, Typeable t, Show t) => FromJSON (Interval t) where
  parseJSON (Array a)
    | V.length a == 2 = parseInterval a
    | otherwise       = fail $ "array should be 2 elements long"
    where
      parseInterval x =
        Interval <$> parseJSON (x ! 0) <*> parseJSON (x ! 1)
  parseJSON v =
    fail $ "expecting Array, but got: " ++ show v

instance (Typeable t, ToJSON t, Show t) => ToJSON (Interval t) where
  toJSON (Interval begin end) = Array $ V.fromList [toJSON begin, toJSON end]

 -- FIXME: write appropriate instance sometime
instance FromField (Interval t) where fromField = undefined

instance ToField   (Interval UTCTime) where
  toField (Interval begin end)
    = Plain $ inQuotes $ Builder.fromString
    $ printf "[%s,%s]"
      (formatTime defaultTimeLocale "%0Y-%m-%d %T+00" begin)
      (formatTime defaultTimeLocale "%0Y-%m-%d %T+00" end)

instance ToField   (Interval Day) where
  toField (Interval begin end)
    = Plain $ inQuotes $ Builder.fromString
    $ printf "[%s,%s]"
      (formatTime defaultTimeLocale "%0Y-%m-%d" begin)
      (formatTime defaultTimeLocale "%0Y-%m-%d" end)


-- | Time difference with minute precision.
--
-- To/FromJSON instances use @"HH:MM"@ string format.
newtype HMDiffTime = HMDiffTime DiffTime deriving (FromField, ToField,
                                                   Typeable)

instance FromJSON HMDiffTime where
  parseJSON (Aeson.String hm) =
     case (map T.decimal $ T.splitOn ":" hm) of
        [Right (hours, _), Right (minutes, _)] ->
            if (0 <= hours && 0 <= minutes && minutes <= 59)
            then return $ HMDiffTime $
                 fromInteger (hours * 60 + minutes) * 60
            else err
        _ -> err
     where
       err = fail $ "Invalid HMDiffTime format: " ++ show hm
  parseJSON _ = fail $ "HMDiffTime JSON must be a string"

instance ToJSON HMDiffTime where
  toJSON (HMDiffTime dt) =
    Aeson.String $ T.concat [ts h, ":", ts m]
    where
      ts = T.pack . show
      (h, m, _) = diffTimeTohms dt


-- | Int wrapper which instructs CaRMa client to use JSON integers in
-- commits.
--
-- This is the preferred integer type for use with new models until
-- string-wrapped integers are no more used anywhere on the client.
newtype TInt = TInt Int deriving (Eq, Show,
                                  FromField, ToField,
                                  FromJSON, ToJSON, Typeable)


-- | List of model instance identifiers. Used to accomodate client
-- pull-children behaviour.
data IdentList m = RL (Vector (IdentI m))
                       deriving (Typeable)

instance ToField (IdentList m) where
    toField (RL v) = toField v

instance Typeable m => FromField (IdentList m) where
    fromField f s = RL <$> fromField f s

instance ToJSON (IdentList m) where
    toJSON (RL v) = toJSON v

instance FromJSON (IdentList m) where
    parseJSON v = RL <$> parseJSON v


-- default filed view
instance DefaultFieldView t =>
         DefaultFieldView (Maybe t) where
  defaultFieldView (_ :: m -> FF (Maybe t) nm desc a)
    = defaultFieldView (undefined :: m -> FF t nm desc a)

instance DefaultFieldView UTCTime where
  defaultFieldView f = (defFieldView f)
    { fv_type = "UTCTime"
    , fv_meta
      = Map.insert "regexp" "datetime"
      $ Map.insert "widget" "datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView HMDiffTime where
  defaultFieldView f = (defFieldView f)
    { fv_type = "text"
    , fv_meta
      = Map.insert "regexp" "timespan"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Bool where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Bool"
    , fv_meta
      = Map.insert "widget" "checkbox"
      $ fv_meta $ defFieldView f
    }


instance DefaultFieldView TInt where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Integer"
    , fv_meta
      = Map.insert "regexp" "number"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance Model m => DefaultFieldView (IdentList m) where
  defaultFieldView f = (defFieldView f)
    { fv_type = "IdentList"
    , fv_meta
      = Map.insert "model" (Aeson.String $ modelName (modelInfo :: ModelInfo m))
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Double where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Double"
    , fv_meta
      = Map.insert "regexp" "double"
      $ Map.insert "widget" "text"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int where
  defaultFieldView f = (defFieldView f)
    {fv_type = "int"
    ,fv_meta
      = Map.insert "sqltype" (Aeson.String "integer")
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView Int16 where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Int32 where
  defaultFieldView f = (defFieldView f) {fv_type = "int"}

instance DefaultFieldView Text where
  defaultFieldView f = (defFieldView f) {fv_type = "text"}

instance DefaultFieldView Day where
  defaultFieldView f = (defFieldView f)
    { fv_type = "Day"
    , fv_meta
      = Map.insert "regexp" "date"
      $ Map.insert "widget" "date"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView PickerField where
  defaultFieldView f = (defFieldView f) {fv_type = "picker"}

instance DefaultFieldView MapField where
  defaultFieldView f = (defFieldView f) {fv_type = "map"}

instance DefaultFieldView Reference where
  defaultFieldView f = (defFieldView f) {fv_type = "reference"}

instance DefaultFieldView Checkbox where
  defaultFieldView f = (defFieldView f) {fv_type = "checkbox"}

instance DefaultFieldView LegacyDate where
  defaultFieldView f = (defFieldView f) {fv_type = "date"}

instance DefaultFieldView LegacyDatetime where
  defaultFieldView f = (defFieldView f) {fv_type = "datetime"}

instance DefaultFieldView JsonAsText where
  defaultFieldView f = (defFieldView f) {fv_type = "JsonAsText"}

instance DefaultFieldView Aeson.Value where
  defaultFieldView f = (defFieldView f) {fv_type = "JSON"}

instance DefaultFieldView Password where
  defaultFieldView f = (defFieldView f) {fv_type = "password"}

instance DefaultFieldView Phone where
  defaultFieldView f = (defFieldView f)
    {fv_type = "phone"
    ,fv_meta
      = Map.insert "regexp" "phone"
      $ Map.insert "picker" "callPlease"
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Int tag) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "dictionaryType" "ModelDict"
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance Typeable tag => DefaultFieldView (Ident Text tag) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary"
    ,fv_meta
      = Map.insert "dictionaryName" (Aeson.String $ typeName (undefined :: tag))
      $ Map.insert "bounded" (Aeson.Bool True)
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector Text) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "dictionary-set-text"
    ,fv_meta
      = Map.insert "widget" "dictionary-many"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Ident Int tag) =>
 DefaultFieldView (Vector (Ident Int tag)) where
  defaultFieldView (_ :: m -> FF (Vector (Ident Int tag)) nm desc app) =
    let v = defaultFieldView (undefined :: m -> FF (Ident Int tag) nm desc app)
    in v{fv_type = "dictionary-set-int"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }

instance DefaultFieldView (Ident Text tag) =>
 DefaultFieldView (Vector (Ident Text tag)) where
  defaultFieldView (_ :: m -> FF (Vector (Ident Text tag)) nm desc app) =
    let v = defaultFieldView (undefined :: m -> FF (Ident Text tag) nm desc app)
    in v{fv_type = "dictionary-set-text"
        ,fv_meta = Map.insert "widget" "dictionary-many" $ fv_meta v
        }


instance DefaultFieldView (Interval UTCTime) where
  defaultFieldView f = (defFieldView f) {fv_type = "interval-datetime"}

instance DefaultFieldView (Interval Day) where
  defaultFieldView f = (defFieldView f)
    {fv_type = "interval-date"
    ,fv_meta
      = Map.insert "widget" "interval-datetime"
      $ fv_meta $ defFieldView f
    }

instance DefaultFieldView (Vector t) => DefaultFieldView (Vector (Maybe t))
  where
  defaultFieldView (_ :: m -> FF (Vector (Maybe t)) nm desc a) =
   defaultFieldView (undefined :: m -> FF (Vector t) nm desc a)

-- | Used to describe event type in Event model
--
-- Represented as "EventType" enum type in postgres
--
-- FIXME: Find a way to check mapping from hs to pg type
data EventType = Login | Logout | Create | Update
               deriving (Eq, Show, Read, Typeable, Generic)

instance FromJSON EventType
instance ToJSON   EventType

instance FromField EventType where
  fromField f mdata = do
    typname <- typename f
    case typname /= "EventType" of
      True  -> returnError Incompatible f ""
      False -> case B.unpack `fmap` mdata of
        Nothing  -> returnError UnexpectedNull f ""
        Just v   -> case readMaybe v of
          Nothing -> returnError ConversionFailed f "mismatched enums"
          Just v' -> return v'

instance ToField EventType where
  toField = toField . show

instance DefaultFieldView EventType where
  defaultFieldView f = (defFieldView f)

data UserStateVal = LoggedOut | Ready | Rest | Busy | Dinner | ServiceBreak
                  deriving (Eq, Enum, Bounded, Show, Read, Typeable, Generic)

instance FromJSON UserStateVal
instance ToJSON   UserStateVal

-- Need this because client send "" instead of null in case of empty
-- which can'd be parsed to 'UserStateVal'
instance FromJSON (Maybe UserStateVal) where
  parseJSON o =
    case fromJSON o of
      Success v -> return $ Just v
      _err      -> return Nothing

instance ToField UserStateVal where
  toField = toField . show

instance FromField UserStateVal where
  fromField f mdata = do
    typname <- typename f
    case typname /= "UserStateVal" of
      True  -> returnError Incompatible f ""
      False -> case B.unpack `fmap` mdata of
        Nothing  -> returnError UnexpectedNull f ""
        Just v   -> case readMaybe v of
          Nothing -> returnError ConversionFailed f "mismatched enums"
          Just v' -> return v'

instance DefaultFieldView UserStateVal where
  defaultFieldView f = (defFieldView f)
    { fv_type = "dictionary"
    , fv_meta = Map.union (fv_meta (defFieldView f)) $ Map.fromList
                [("dictionaryType", "ComputedDict")
                ,("dictionaryName", "UserStateVal")
                ,("bounded", "true")
                ]
    }


-- | PostGIS coordinates type (longitude and latitude as doubles in
-- WGS 84).
--
-- ToField/FromField instances assume that bytestring data is
-- hex-encoded little-endian WKB for 2D point with SRID=4326.
-- (http://trac.osgeo.org/postgis/browser/trunk/doc/ZMSgeoms.txt)
newtype Coords = Coords (Double, Double)
                 deriving (Show, Typeable)

instance FromField Coords where
    fromField f Nothing = returnError UnexpectedNull f "No coordinates"
    fromField f (Just s) = case S.decode $ fst $ B16.decode s of
                             Left err -> returnError ConversionFailed f err
                             Right v -> return v

instance ToField Coords where
    toField c = Escape $ B16.encode $ S.encode c

instance Serialize Coords where
    get = do
      -- Skip byte order mark
      skip 1
      -- Skip type annotation
      skip 4
      -- Skip SRID value
      skip 4
      -- Read coordinates
      x <- getFloat64le
      y <- getFloat64le
      return $ Coords (x, y)
    put (Coords (x, y)) = do
      -- LE
      putWord8 1
      -- Point with SRID flag
      putWord32le 0x20000001
      -- SRID=4326
      putWord32le 0x000010E6
      -- Write coordinates
      putFloat64le x
      putFloat64le y


instance FromJSON Coords where
  parseJSON (String s)
    = case rights $ map (T.signed T.double) $ T.splitOn "," s of
      [(x,""), (y,"")] -> return $ Coords (x, y)
      _                -> fail $ "invalid Coords: " ++ show s
  parseJSON v = fail $ "invalid Coords: " ++ show v


instance ToJSON Coords where
  toJSON (Coords (x,y)) = String $ T.pack $ show x ++ "," ++ show y


instance DefaultFieldView Coords where
  defaultFieldView f = (defFieldView f) {fv_type = "coords"}


typeName :: forall t . Typeable t => t -> Text
typeName _ = T.pack $ tyConName $ typeRepTyCon $ typeOf (undefined :: t)


defFieldView
  :: forall nm desc a m t
  .  (SingI nm, SingI desc, FieldKindSing a)
  => (m -> FF t nm desc a) -> FieldView
defFieldView f = FieldView
  {fv_name = fieldName f
  ,fv_type = "undefined"
  ,fv_canWrite = True
  ,fv_meta = Map.fromList
    [("label",    Aeson.String $ fieldDesc f)
    ,("app",      Aeson.String fieldKind)
    ]
  }
  where
    fieldKind = case fieldKindSing :: FieldKindSingleton a of
      FKSDefault   -> "default"
      FKSEphemeral -> "ephemeral"


instance DefaultFieldView DiffTime where
  defaultFieldView f = (defFieldView f)
    { fv_type = "DiffTime"
    , fv_meta
      = Map.insert "widget" "datetime"
      $ fv_meta $ defFieldView f
    }

diffTimeTohms :: Real a => a -> (Int, Int, Int)
diffTimeTohms t =
  let ss :: Int = floor $ toRational t
      sec = ss `rem` 60
      mis = (ss `div` 60) `rem` 60
      hrs = (ss `div` 60) `div` 60
  in (hrs, mis, sec)

printDiffTime :: DiffTime -> String
printDiffTime t = let (hrs, mis, sec) = diffTimeTohms t
                  in printf "%d:%.2d:%.2d" hrs mis sec

instance ToJSON DiffTime where
  toJSON t = Aeson.String $ T.pack $ printDiffTime t

instance FromJSON DiffTime where
  parseJSON (Aeson.String s) =
    case parseDiffTime $ T.encodeUtf8 s of
      Left err -> fail $ show err
      Right d  -> return d
  parseJSON o = fail $ "DiffTime parser: expecting string, but got" ++ show o

instance ToField DiffTime where
  toField t = Plain $ inQuotes $ Builder.fromString $ printDiffTime t

instance FromField DiffTime where
  fromField f mdata = if typeOid f /= typoid interval
    then returnError Incompatible f ""
    else case parseDiffTime `fmap` mdata of
      Just (Right d)  -> return d
      Just (Left err) -> returnError ConversionFailed f (show err)
      Nothing         -> returnError UnexpectedNull f ""

parseDiffTime :: B.ByteString -> Either ParseError DiffTime
parseDiffTime = parse diffTime "DiffTime"
  where
    diffTime = do
      d <- optionMaybe $ try $ days
      void $ optional space
      t <- optionMaybe $ try time
      case (d, t) of
        (Nothing, Nothing) -> fail "can't parse interval"
        _ -> return $ secondsToDiffTime $ (fromMaybe 0 d) + (fromMaybe 0 t)
    days = do
      mi    <- minus
      ds    <- manyTill digit (try (string " day"))
      void $ optional $ char 's'
      case readMaybe ds of
        Just d -> return $ mi * d * 60 * 60 * 24
        _           -> fail "no days found"
    time = do
      mi <- minus
      ds <- many1 digit `sepBy` char ':' >>= return . map (readMaybe)
      case sequence ds of
        Just [h :: Integer, m, s] -> return $  mi * (s + m*60 + h*3600)
        _                         -> fail "no time found"
    minus = option 1 $ char '-' *> return (-1 :: Integer)
