{-# LANGUAGE OverloadedStrings #-}

{-|

carma-models-style dictionaries interface

-}

module Data.Dict.New
    ( NewDict
    , PreDict
    , loadNewDict
    , loadNewDict'
    , labelOfValue
    , valueOfLabel
    )

where

import Data.Aeson
import Data.Attoparsec.Number

import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Vector as V


-- | Pre-dictionary parsed from JSON (@/_/DictName/@ response).
type PreDict = V.Vector (HM.HashMap Text Value)


-- | Similar to 'Dict', but with integer values and Text labels.
newtype NewDict = NewDict ( HM.HashMap Int Text
                          , HM.HashMap Text Int
                          )


-- | Standard 'loadNewDict' version with @id@ as the value key and
-- @label@ as the label key.
loadNewDict' :: PreDict
             -> NewDict
loadNewDict' = loadNewDict "id" "label"


loadNewDict :: Text
            -- ^ Internal value key.
            -> Text
            -- ^ External label key.
            -> PreDict
            -> NewDict
loadNewDict kv kl entries =
    NewDict $ V.foldl' (\(fm, im) e ->
                        case (HM.lookup kv e, HM.lookup kl e) of
                          (Just (Number (I val)), Just (String lab)) ->
                              (HM.insert v lab fm, HM.insert lab v im)
                                  where
                                    v = fromIntegral val
                          _ -> (fm, im)) (HM.empty, HM.empty) entries


-- | Given value, lookup label in the dictionary.
labelOfValue :: Int
             -- ^ Value.
             -> NewDict
             -> Maybe Text
labelOfValue k (NewDict (fm, _)) = HM.lookup k fm


-- | Given label, lookup value in the dictionary.
valueOfLabel :: Text
             -- ^ Label.
             -> NewDict
             -> Maybe Int
valueOfLabel v (NewDict (_, rm)) = HM.lookup v rm
