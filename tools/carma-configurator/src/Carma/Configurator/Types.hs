{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Carma.Configurator.Types
     ( Email (Email)
     ) where

import           GHC.Generics (Generic)

import           Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Attoparsec.Text

import           Carma.Utils.Operators


newtype Email = Email Text deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Email where
  parseJSON v@(String x)
    = parseOnly parser x
    & \case Left  _ -> typeMismatch "Email" v
            Right y -> pure y

    where parser = -- TODO write some test for this parser
            f <$> takeWhile1 (/= '@') <* char '@' <*> takeText <* endOfInput
            where f a b = Email $ a <> "@" <> b

  parseJSON invalid = typeMismatch "Email" invalid
