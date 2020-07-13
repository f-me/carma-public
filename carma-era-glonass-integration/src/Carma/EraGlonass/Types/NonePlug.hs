{-# LANGUAGE MultiParamTypeClasses #-}

module Carma.EraGlonass.Types.NonePlug
     ( NonePlug (..)
     ) where

import           Data.Proxy
import           Data.Aeson
import           Data.Swagger

import           Servant


-- | A plug for a response which is just nothingness.
--
-- "Request for service" responses just with HTTP status code,
-- without response body.
data NonePlug = NonePlug deriving (Eq, Show)

instance Semigroup NonePlug where
  NonePlug <> NonePlug = NonePlug

instance Monoid NonePlug where
  mempty = NonePlug

instance MimeRender PlainText NonePlug where
  mimeRender Proxy NonePlug = mempty

instance MimeRender JSON NonePlug where
  mimeRender Proxy NonePlug = encode Null

instance ToSchema NonePlug where
  declareNamedSchema _
    = pure $ NamedSchema Nothing mempty
    { _schemaParamSchema
        = (_schemaParamSchema mempty)
        { _paramSchemaDefault = Just Null
        , _paramSchemaEnum    = Just [Null]
        }
    }
