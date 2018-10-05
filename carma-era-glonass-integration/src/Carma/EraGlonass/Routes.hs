{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Carma.EraGlonass.Routes
     ( type IncomingAPI
     , type OutcomingAPI
     ) where

import           Servant

import           Carma.EraGlonass.Types


{-|
Routes which CaRMa provides for outside requests by Era Glonass.

* Integration point with code __EG.CRM.01__.

  @POST \/calls/status@

  Era Glonass supposed to upload data about its \"call card" to this
  route. Route path written this way to look just as their route used for
  working with \"call cards".

  Triggered at any time (when related action happens) by Era Glonass.
-}
type IncomingAPI
   = -- EG.CRM.01
     -- POST /calls/status
     "calls" :> "status" :> ReqBody '[JSON] EGCreateCallCardRequest
                         :> Post    '[JSON] EGCreateCallCardResponse


{-|
Routes which CaRMa uses to make requests to Era Glonass.

* Integration point with code __CRM.EG.02__.

  * @DELETE \/providers/vehicles@ -
      To delete a VIN from list of VINs handled by CaRMa;

  * @PUT \/providers/vehicles@ -
      To add a VIN to list of VINs handled by CaRMa;

  * @POST \/providers/vehicles@ -
      To check if VIN is added and handled by CaRMa.

  Uploading CaRMa 'Carma.Model.Contract.Persistent.Contract's to Era Glonass.
  Only those that are have 'Carma.Model.SubProgram.Persistent.SubProgram'
  which is __/@eraGlonassParticipant@/__.

  Triggered daily on schedule by CaRMa.

* Integration point with code __CRM.EG.03__.

  * @PUT \/calls/status@ - To add data about customer, to close a Call Card
                           (currently not implemented);

  * @POST \/calls/status@ - To update status of a Call Card.

  Uploading new CaRMa 'Carma.Model.Case.Persistent.Case' data
  (for \"call card" of Era Glonass service).

  Triggered at any time (when related action happens) by CaRMa.
-}
type OutcomingAPI
    =  -- CRM.EG.02
       -- DELETE, PUT, POST /providers/vehicles
       "providers" :> "vehicles" :> (    ReqBody   '[JSON] EGDeleteVinRequest
                                         :> Delete '[JSON] EGDeleteVinResponse

                                    :<|> ReqBody '[JSON] EGAddVinRequest
                                         :> Put  '[JSON] EGAddVinResponse

                                    :<|> ReqBody '[JSON] EGCheckVinRequest
                                         :> Post '[JSON] EGCheckVinResponse
                                    )

  :<|> -- CRM.EG.03
       -- PUT, POST /calls/status
       "calls" :> "status" :> ReqBody '[JSON] EGUpdateCallCardStatusRequest
                           :> Post    '[JSON] EGUpdateCallCardStatusResponse
