{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import           Data.Proxy
import           Data.String (fromString)
import           Data.Function ((&))
import           Data.Swagger (Swagger)

import qualified Network.Wai.Handler.Warp as Warp

import           Servant
import           Servant.Swagger (toSwagger)

import           Carma.EraGlonass.Routes


type ServerAPI
    =  IncomingAPI
  :<|> "debug" :> "swagger" :> Get '[JSON] Swagger


main :: IO ()
main = Warp.runSettings warpSettings app

  where app = serve (Proxy :: Proxy ServerAPI) server

        port = 8166                   :: Warp.Port
        host = fromString "127.0.0.1" :: Warp.HostPreference

        warpSettings
          = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setHost host


server :: Server ServerAPI
server = egCRM01 :<|> swagger


egCRM01 :: Handler ()
egCRM01 = pure ()


swagger :: Handler Swagger
swagger = pure $ toSwagger (Proxy :: Proxy IncomingAPI)
