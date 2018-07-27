{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)

import           Control.Monad.Reader (runReaderT)

import           Servant
import           Servant.Swagger (toSwagger)

import           Carma.EraGlonass.Routes
import           Carma.EraGlonass.Types


type ServerAPI
    =  IncomingAPI
  :<|> -- GET /debug/swagger.json
       "debug" :> "swagger.json" :> Get '[JSON] Swagger


serverApplicaton :: AppContext -> Application
serverApplicaton appContext
  = serve (Proxy :: Proxy ServerAPI)
  $ server appContext


server :: AppContext -> Server ServerAPI
server appContext = (\req -> wrap $ egCRM01 req) :<|> wrap swagger
  where wrap = flip runReaderT appContext


egCRM01 :: Applicative m => EGCreateCallCardRequest -> m ()
egCRM01 _ = pure ()


swagger :: Applicative m => m Swagger
swagger = pure $ toSwagger (Proxy :: Proxy IncomingAPI)
