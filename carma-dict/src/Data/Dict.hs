{-# LANGUAGE OverloadedStrings #-}

{-|

Load and query CaRMa dictionaries.

-}

module Data.Dict
    ( Dict
    , loadDict
    , labelOfValue
    , valueOfLabel
    )

where

import Control.Monad
import Data.Functor

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V


-- | Dictionary is a mapping between values (keys) and labels
-- (values).
--
-- For convenience both values and labels a represented as
-- ByteStrings.
--
-- 'FromJSON' instance provides interface to read dictionaries from
-- CaRMa format for dictionaries. Dictionary is a JSON object, where
-- @entries@ key contains a list of mapping objects with @value@ and
-- @label@ fields:
--
-- > {"entries":[{"value": "v1", "label": "Label for v1"}, ..]}
newtype Dict = Dict ( HM.HashMap BS.ByteString BS.ByteString
                    , HM.HashMap BS.ByteString BS.ByteString
                    )
    deriving Show

instance FromJSON Dict where
    parseJSON (Object v) =
        do
          entries <- v .: "entries"
          -- Read mapping objects in entries list and fill forward and
          -- inverse mappings for Dict.
          (fm, im)  <- V.foldM' (\(fm, im) e -> do
                                   val <- e .: "value"
                                   lab <- e .: "label"
                                   return $ (HM.insert val lab fm,
                                             HM.insert lab val im))
                       (HM.empty, HM.empty) entries
          return $ Dict (fm, im)
    parseJSON _          = mzero


-- | Load dictionary from JSON file. Return Nothing if parsing failed.
loadDict :: FilePath -> IO (Maybe Dict)
loadDict file = decode' <$> BSL.readFile file


-- | Given value, lookup label in the dictionary.
labelOfValue :: BS.ByteString
             -- ^ Value.
             -> Dict
             -> Maybe BS.ByteString
labelOfValue k (Dict (fm, _)) = HM.lookup k fm


-- | Given label, lookup value in the dictionary.
valueOfLabel :: BS.ByteString
             -- ^ Label.
             -> Dict
             -> Maybe BS.ByteString
valueOfLabel v (Dict (_, rm)) = HM.lookup v rm
