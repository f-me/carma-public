{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- Incoming server implementation to provide an API for Era Glonass side
-- and also some debug stuff for internal usage.
module Carma.EraGlonass.Server
     ( serverApplicaton
     ) where

import           Data.Proxy
import           Data.Swagger (Swagger)

import           Servant
import           Servant.Swagger (toSwagger)

import           Carma.EraGlonass.Routes


type ServerAPI
    =  IncomingAPI
  :<|> "debug" :> "swagger" :> Get '[JSON] Swagger


serverApplicaton :: Application
serverApplicaton = serve (Proxy :: Proxy ServerAPI) server


server :: Server ServerAPI
server = egCRM01 :<|> swagger


egCRM01 :: Handler ()
egCRM01 = pure ()


swagger :: Handler Swagger
swagger = pure $ toSwagger (Proxy :: Proxy IncomingAPI)
