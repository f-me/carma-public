{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Carma.EraGlonass.Routes
     ( type IncomingAPI
     , type OutcomingAPI
     ) where

import           Servant

import           Carma.EraGlonass.Types


-- | Routes which CaRMa provides for outside requests by Era Glonass.
--
-- * Integration point with code __EG.CRM.01__.
--
--   @POST \/calls/status@
--
--   Era Glonass supposed to upload data about its \"call card" to this
--   route. Route path written this way to look just as their route used for
--   working with \"call cards".
--
--   Triggered at any time (when related action happens) by Era Glonass.
--
type IncomingAPI
   = -- EG.CRM.01
     -- POST /calls/status
     "calls" :> "status" :> ReqBody '[JSON] EGCreateCallCardRequest
                         :> Post    '[JSON] EGCreateCallCardResponse


-- | Routes which CaRMa uses to make requests to Era Glonass.
--
-- * Integration point with code __CRM.EG.02__.
--
--   @DELETE, PUT, POST \/providers/vehicles@
--
--   Uploading CaRMa 'Carma.Model.Contract.Persistent.Contract's to Era Glonass.
--   Only those that are have 'Carma.Model.SubProgram.Persistent.SubProgram'
--   which is __/@eraGlonassParticipant@/__.
--
--   Triggered daily on schedule by CaRMa.
--
-- * Integration point with code __CRM.EG.03__.
--
--   @PUT, POST \/calls/status@
--
--   Uploading new CaRMa 'Carma.Model.Case.Persistent.Case' data
--   (for \"call card" of Era Glonass service).
--
--   Triggered at any time (when related action happens) by CaRMa.
--
type OutcomingAPI
    =  -- CRM.EG.02
       -- DELETE, PUT, POST /providers/vehicles
       "providers" :> "vehicles" :> (    Delete '[JSON] ()
                                    :<|> Put    '[JSON] ()
                                    :<|> Post   '[JSON] ()
                                    )

  :<|> -- CRM.EG.03
       -- PUT, POST /calls/status
       "calls" :> "status" :> (    Put  '[JSON] ()
                              :<|> Post '[JSON] ()
                              )
