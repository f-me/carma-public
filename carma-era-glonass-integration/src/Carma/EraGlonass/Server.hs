{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)

import           Control.Monad.Reader (MonadReader, runReaderT)

import           Servant
import           Servant.Swagger (toSwagger)

import           Carma.Monad.LoggerBus
import           Carma.EraGlonass.Logger () -- instance
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


egCRM01
  :: ( MonadReader AppContext m
     , MonadLoggerBus m
     )
  => EGCreateCallCardRequest
  -> m EGCreateCallCardResponse
egCRM01 _ = do
  logError "TODO EG.CRM.01"

  pure EGCreateCallCardResponse
    { responseId = "foo"
    , cardidProvider = "bar"
    , acceptId = "baz"
    , requestId = "9db7cf43-deab-4c27-a8df-74bec0b75df1"
    , acceptCode = OK
    , statusDescription = Nothing
    }


swagger :: Applicative m => m Swagger
swagger = pure $ toSwagger (Proxy :: Proxy IncomingAPI)
