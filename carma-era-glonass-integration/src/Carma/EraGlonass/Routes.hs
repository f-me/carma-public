{-# LANGUAGE ExplicitNamespaces, TypeOperators, DataKinds #-}

module Carma.EraGlonass.Routes
     ( type IncomingAPI
     , type OutcomingAPI
     ) where

import           Servant

import           Carma.EraGlonass.Types.NonePlug (type NonePlug)
import           Carma.EraGlonass.Types.EGMayFailToParse (type EGMayFailToParse)
import           Carma.EraGlonass.Types.EGRequestForServiceRequest
                   ( type EGRequestForServiceRequest
                   )
import           Carma.EraGlonass.Types.EGBindVehiclesRequest
                   ( type EGBindVehiclesRequest
                   , type EGBindVehiclesResponse
                   , type EGBindVehiclesMode (..)
                   )
import           Carma.EraGlonass.Types.EGChangeProcessingStatusRequest
                   ( type EGChangeProcessingStatusRequest
                   , type EGChangeProcessingStatusResponse
                   )
import           Carma.EraGlonass.Types.EGChangeRequestStatusRequest
                   ( type EGChangeRequestStatusRequest
                   )


type (#) = (:<|>); infixr 3 #


{-|
Routes which CaRMa provides for requests came from Era Glonass outside world.

* @POST \<url>/requestForService@

  Era Glonass is supposed to upload data about its \"call card" to this
  route. CaRMa is supposed to create its 'Carma.Model.Case.Persistent.Case'
  filling its fields with data from received \"call card" or \"request for
  commercial service formed by call card".

  Triggered at any time (when related action happens) by Era Glonass.

  It serves two purposes:
    1. Request for commercial service;
    2. Update attributes of previous service request.

  This route supposed to answer only with HTTP status code,
  without any response body.
-}
type IncomingAPI
   = -- POST <url>/requestForService
     "requestForService"
       :> ReqBody '[JSON] (EGMayFailToParse EGRequestForServiceRequest)
       :> Post    '[JSON] NonePlug


{-|
Routes which CaRMa uses to make requests to Era Glonass.

* * @POST \<url>/bindVehicles@ -
      To bind or unbind VIN(s) from list of VINs handled by CaRMa;

  Uploading CaRMa 'Carma.Model.Contract.Persistent.Contract's to Era Glonass.
  Only those which have 'Carma.Model.SubProgram.Persistent.SubProgram'
  which is __/@eraGlonassParticipant@/__.

  Triggered daily on schedule by CaRMa.

* * @POST \<url>/changeProcessingStatus@ -
      To close a service in processing.

  * @POST \<url>/changeRequestStatus@ -
      To update a status of a request for service.

  Uploading new CaRMa 'Carma.Model.Case.Persistent.Case' data
  (for \"call card" of Era Glonass service).

  Triggered at any time (when related action happens) by CaRMa.

  Keep in mind that @EGChangeRequestStatusRequest@ shares
  @EGChangeProcessingStatusResponse@ type with @EGChangeProcessingStatusRequest@
  because they have the same response scheme.
-}
type OutcomingAPI
   = -- POST <url>/bindVehicles
     "bindVehicles" :>
       ( ReqBody '[JSON] (EGBindVehiclesRequest 'Bind)
         :> Post '[JSON] (EGMayFailToParse (EGBindVehiclesResponse 'Bind))

       # ReqBody '[JSON] (EGBindVehiclesRequest 'Unbind)
         :> Post '[JSON] (EGMayFailToParse (EGBindVehiclesResponse 'Unbind))
       )

   # -- POST <url>/changeProcessingStatus
     "changeProcessingStatus"
       :> ReqBody '[JSON] [EGChangeProcessingStatusRequest]
       :> Post    '[JSON] (EGMayFailToParse EGChangeProcessingStatusResponse)

   # -- POST <url>/changeRequestStatus
     "changeRequestStatus"
       :> ReqBody '[JSON] [EGChangeRequestStatusRequest]
       :> Post    '[JSON] (EGMayFailToParse EGChangeProcessingStatusResponse)
