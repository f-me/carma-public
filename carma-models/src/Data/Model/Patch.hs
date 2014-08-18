{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Patch
  ( Patch(Patch), untypedPatch
  , get, get', put, delete
  , empty
  , W(..)
  )

where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Reader (ask)
import Control.Monad (mplus)
import Control.Monad.Trans.Class (lift)

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, toLower)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.PostgreSQL.Simple.Internal ( Row(..)
                                           , RowParser(..)
                                           , conversionError)
import Database.PostgreSQL.Simple.FromField (ResultError(..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

import Data.Maybe (fromJust)
import Data.Dynamic
import GHC.TypeLits

import Data.Model


data Patch m
  = Patch { untypedPatch :: HashMap Text Dynamic }
  deriving Typeable

get :: (Typeable t, SingI name) =>
       Patch m -> (m -> Field t (FOpt name desc app)) -> Maybe t
get (Patch m) f
  = fromJust . fromDynamic
  <$> HashMap.lookup (fieldName f) m


put :: (Typeable t, SingI name) =>
       (m -> Field t (FOpt name desc app)) -> t -> Patch m -> Patch m
put f v (Patch m) = Patch $ HashMap.insert (fieldName f) (toDyn v) m


empty :: Patch m
empty = Patch HashMap.empty


get' :: (Typeable t, SingI name) =>
        Patch m -> (m -> Field t (FOpt name desc app)) -> t
get' p f
  = fromJust $ get p f


delete :: (SingI name) =>
          (m -> Field t (FOpt name desc app)) -> Patch m -> Patch m
delete f (Patch m) = Patch $ HashMap.delete (fieldName f) m


instance Model m => FromJSON (Patch m) where
  parseJSON (Object o)
    = Patch . HashMap.fromList
    <$> mapM parseField (HashMap.toList o)
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      parseField (name, val) = case HashMap.lookup name fields of
        Nothing -> fail $ "Unexpected field: " ++ show name
        Just p  -> (name,) <$> fd_parseJSON p val
  parseJSON j = fail $ "JSON object expected but here is what I have: " ++ show j


instance Model m => ToJSON (Patch m) where
  toJSON (Patch m) = object [(k, toJS k v) | (k,v) <- HashMap.toList m]
    where
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      toJS k = fd_toJSON $ fields HashMap.! k


instance Model m => FromRow (Patch m) where
  fromRow = Patch . HashMap.fromList <$> sequence
    [(fd_name f,) <$> fd_fromField f
    | f <- onlyDefaultFields $ modelFields (modelInfo :: ModelInfo m)
    ]

instance Model m => ToRow (Patch m) where
  toRow (Patch m) = concatMap fieldToRow $ HashMap.toList m
    where
      -- NB. Please note that field order is significant
      -- it MUST match with the one in Patch.Sql.insert
      fields = modelFieldsMap (modelInfo :: ModelInfo m)
      fieldToRow (nm, val) = case HashMap.lookup nm fields of
        Just f@(FieldDesc{}) -> [fd_toField f val]
        Just _  -> [] -- skip ephemeral field
        Nothing -> [] -- skip unknown fields to allow upcasting child models

newtype W m = W { unW :: m }

-- | Special instance which can build patch retrieving fields by their names
instance Model m => FromRow (W (Patch m)) where
  fromRow = do
    n  <- numFieldsRemaining
    fs <- map decodeUtf8 <$> catMaybes <$> mapM fname [0..n-1]
    case filter (\(_, f) -> not $ hasField f) $ zip fs $ fields fs of
      [] -> W . Patch . HashMap.fromList <$> sequence
            [(fd_name f,) <$> fd_fromField f | f <- catMaybes $ fields fs]
      errs -> RP $ lift $ lift $ conversionError $
              ConversionFailed  "" Nothing "" "" $
        "Can't find this fields in model: " ++ (show $ map fst errs)
    where
      fM = modelFieldsMap (modelInfo :: ModelInfo m)
      fm = HashMap.foldl'
           (\a f -> HashMap.insert (toLower $ fd_name f) f a) HashMap.empty fM
      fields = map (\n -> HashMap.lookup n fM `mplus` HashMap.lookup n fm)
      hasField (Just _) = True
      hasField Nothing  = False

fname :: Int -> RowParser (Maybe ByteString)
fname n = RP $ do
  Row{..} <- ask
  return $ unsafeDupablePerformIO $ PQ.fname rowresult (PQ.toColumn n)

instance Model m => ToJSON (W (Patch m)) where
  toJSON (W p) = toJSON p
