{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Carma.EraGlonass.Routes
     ( type IncomingAPI
     , type OutcomingAPI
     ) where

import           Servant

import           Carma.EraGlonass.Types


-- Routes which CaRMa provides for outside requests by Era Glonass.
type IncomingAPI
   = -- Integration point with code "EG.CRM.01".
     --
     -- Era Glonass supposed to upload data about its "call card" to this route.
     -- Route path written this way to look just as their route used for working
     -- with "call cards".
     --
     -- POST /calls/status
     --
     "calls" :> "status" :> ReqBody '[JSON] EGCreateCallCardRequest
                         :> Post    '[JSON] ()


type OutcomingAPI
    =  -- Integration point with code "CRM.EG.02".
       --
       -- Uploading CaRMa contracts to Era Glonass
       -- (ones which marked as taking part in integration with Era Glonass).
       --
       -- DELETE, PUT, POST /providers/vehicles
       "providers" :> "vehicles" :> (    Delete '[JSON] ()
                                    :<|> Put    '[JSON] ()
                                    :<|> Post   '[JSON] ()
                                    )

       -- Integration point with code "CRM.EG.03".
       --
       -- Uploading new CaRMa `Case` data
       -- (for "call card" of Era Glonass service).
       --
       -- PUT, POST /calls/status
  :<|> "calls" :> "status" :> (    Put  '[JSON] ()
                              :<|> Post '[JSON] ()
                              )
