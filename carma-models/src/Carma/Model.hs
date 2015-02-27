{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.Model
  ( Model
  , Ident(..), IdentI, IdentT
  , dispatch
  , modelMap
  )

where

import qualified Data.Map                                as Map
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Typeable

import           Carma.Model.AbuseTarget                 (AbuseTarget)
import           Carma.Model.Action                      (Action)
import           Carma.Model.ActionResult                (ActionResult)
import           Carma.Model.ActionType                  (ActionType)
import           Carma.Model.Attachment                  (Attachment)
import           Carma.Model.AvayaEvent                  (AvayaEvent)
import           Carma.Model.AvayaEventType              (AvayaEventType)
import           Carma.Model.BusinessRole                (BusinessRole)
import           Carma.Model.Call                        (Call)
import           Carma.Model.CallReason                  (CallReason)
import           Carma.Model.CallType                    (CallType)
import           Carma.Model.CallerType                  (CallerType)
import           Carma.Model.CarClass                    (CarClass)
import           Carma.Model.CarMake                     (CarMake)
import           Carma.Model.CarModel                    (CarModel)
import           Carma.Model.Case                        (Case)
import           Carma.Model.CaseStatus                  (CaseStatus)
import           Carma.Model.CheckType                   (CheckType)
import           Carma.Model.City                        (City)
import           Carma.Model.ClientRefusalReason         (ClientRefusalReason)
import           Carma.Model.Colors                      (Colors)
import           Carma.Model.ConstructorFieldOption      (ConstructorFieldOption)
import           Carma.Model.ConsultationType            (ConsultationType)
import           Carma.Model.Contract
import           Carma.Model.ContractCheckStatus
import           Carma.Model.CtrModel                    (CtrModel)
import           Carma.Model.DeferTime                   (DeferTime)

import           Carma.Model.Diagnostics.Cause           (Cause)
import           Carma.Model.Diagnostics.Part            (Part)
import           Carma.Model.Diagnostics.Suggestion      (Suggestion)
import           Carma.Model.Diagnostics.System          (System)
import           Carma.Model.Diagnostics.Wazzup          (Wazzup)

import           Carma.Model.Dictionary                  (Dictionary)
import           Carma.Model.Engine                      (Engine)
import           Carma.Model.FalseCall                   (FalseCall)
import           Carma.Model.FieldPermission             (FieldPermission)

import           Carma.Model.KPI.Group                   (GroupKPI)
import           Carma.Model.KPI.Oper                    (OperKPI)
import           Carma.Model.KPI.Stat                    (StatKPI)

import           Carma.Model.LegalForm
import           Carma.Model.Partner
import           Carma.Model.PartnerCancel               (PartnerCancel)
import           Carma.Model.PartnerRefusalReason        (PartnerRefusalReason)
import           Carma.Model.PartnerService              (PartnerService)
import           Carma.Model.PaymentType                 (PaymentType)
import           Carma.Model.Program                     (Program)
import           Carma.Model.ProgramType
import           Carma.Model.Region                      (Region)
import           Carma.Model.Role                        (Role)
import           Carma.Model.Satisfaction                (Satisfaction)

import           Carma.Model.Service                     (Service)
import           Carma.Model.Service.AverageCommissioner (AverageCommissioner)
import           Carma.Model.Service.Bank                (Bank)
import           Carma.Model.Service.Consultation        (Consultation)
import           Carma.Model.Service.Continue            (Continue)
import           Carma.Model.Service.DeliverCar          (DeliverCar)
import           Carma.Model.Service.DeliverClient       (DeliverClient)
import           Carma.Model.Service.DeliverParts        (DeliverParts)
import           Carma.Model.Service.Hotel               (Hotel)
import           Carma.Model.Service.Information         (Information)
import           Carma.Model.Service.LegalAssistance     (LegalAssistance)
import           Carma.Model.Service.Rent                (Rent)
import           Carma.Model.Service.SoberDriver         (SoberDriver)
import           Carma.Model.Service.Taxi                (Taxi)
import           Carma.Model.Service.Tech                (Tech)
import           Carma.Model.Service.TechInspect         (TechInspect)
import           Carma.Model.Service.Tickets             (Tickets)
import           Carma.Model.Service.Towage              (Towage)
import           Carma.Model.Service.Transportation      (Transportation)
import           Carma.Model.ServiceInfo                 (ServiceInfo)
import           Carma.Model.ServiceStatus               (ServiceStatus)
import           Carma.Model.ServiceType                 (ServiceType)

import           Carma.Model.Sms                         (Sms)
import           Carma.Model.SmsTemplate                 (SmsTemplate)

import           Carma.Model.SubProgram                  (SubProgram)
import           Carma.Model.SubProgramContact
import           Carma.Model.SubProgramContractPermission
import           Carma.Model.SubProgramService           (SubProgramService)

import           Carma.Model.TarifOption                 (TarifOption)
import           Carma.Model.TaxScheme                   (TaxScheme)
import           Carma.Model.TechType                    (TechType)
import           Carma.Model.TowType                     (TowType)
import           Carma.Model.Transmission                (Transmission)
import           Carma.Model.UserState                   (UserState)
import           Carma.Model.Usermeta                    (Usermeta)
import           Carma.Model.VDN                         (VDN)
import           Carma.Model.VinFormat
import           Carma.Model.VipNumber                   (VipNumber)

import           Data.Model


dispatch :: forall a . Text -> (forall m . Model m => m -> a) -> Maybe a
dispatch model fn = Map.lookup model $ modelMap fn


modelMap :: forall a . (forall m . Model m => m -> a) -> Map.Map Text a
modelMap fn = modelMap'
  where
    add :: Model m => m -> (Text, a)
    add m = (T.pack $ show $ typeOf m, fn m)
    modelMap' = Map.fromList
      [add (undefined :: Dictionary)
      ,add (undefined :: AbuseTarget)
      ,add (undefined :: Action)
      ,add (undefined :: ActionResult)
      ,add (undefined :: ActionType)
      ,add (undefined :: Attachment)
      ,add (undefined :: AvayaEvent)
      ,add (undefined :: AvayaEventType)
      ,add (undefined :: AverageCommissioner)
      ,add (undefined :: Bank)
      ,add (undefined :: BusinessRole)
      ,add (undefined :: Call)
      ,add (undefined :: CallReason)
      ,add (undefined :: CallType)
      ,add (undefined :: CallerType)
      ,add (undefined :: CarClass)
      ,add (undefined :: CarMake)
      ,add (undefined :: CarModel)
      ,add (undefined :: Case)
      ,add (undefined :: CaseStatus)
      ,add (undefined :: Cause)
      ,add (undefined :: CheckType)
      ,add (undefined :: City)
      ,add (undefined :: ClientRefusalReason)
      ,add (undefined :: Colors)
      ,add (undefined :: ConstructorFieldOption)
      ,add (undefined :: Consultation)
      ,add (undefined :: ConsultationType)
      ,add (undefined :: Continue)
      ,add (undefined :: Contract)
      ,add (undefined :: ContractCheckStatus)
      ,add (undefined :: CtrModel)
      ,add (undefined :: DeferTime)
      ,add (undefined :: DeliverCar)
      ,add (undefined :: DeliverClient)
      ,add (undefined :: DeliverParts)
      ,add (undefined :: Engine)
      ,add (undefined :: FalseCall)
      ,add (undefined :: FieldPermission)
      ,add (undefined :: GroupKPI)
      ,add (undefined :: Hotel)
      ,add (undefined :: Information)
      ,add (undefined :: LegalAssistance)
      ,add (undefined :: LegalForm)
      ,add (undefined :: OperKPI)
      ,add (undefined :: Part)
      ,add (undefined :: Partner)
      ,add (undefined :: PartnerCancel)
      ,add (undefined :: PartnerRefusalReason)
      ,add (undefined :: PartnerService)
      ,add (undefined :: PaymentType)
      ,add (undefined :: Program)
      ,add (undefined :: ProgramType)
      ,add (undefined :: Region)
      ,add (undefined :: Rent)
      ,add (undefined :: Role)
      ,add (undefined :: Satisfaction)
      ,add (undefined :: Service)
      ,add (undefined :: ServiceInfo)
      ,add (undefined :: ServiceStatus)
      ,add (undefined :: ServiceType)
      ,add (undefined :: Sms)
      ,add (undefined :: SmsTemplate)
      ,add (undefined :: SoberDriver)
      ,add (undefined :: StatKPI)
      ,add (undefined :: SubProgram)
      ,add (undefined :: SubProgramContact)
      ,add (undefined :: SubProgramContractPermission)
      ,add (undefined :: SubProgramService)
      ,add (undefined :: Suggestion)
      ,add (undefined :: System)
      ,add (undefined :: TarifOption)
      ,add (undefined :: TaxScheme)
      ,add (undefined :: Taxi)
      ,add (undefined :: Tech)
      ,add (undefined :: TechInspect)
      ,add (undefined :: TechType)
      ,add (undefined :: Tickets)
      ,add (undefined :: TowType)
      ,add (undefined :: Towage)
      ,add (undefined :: Transmission)
      ,add (undefined :: Transportation)
      ,add (undefined :: UserState)
      ,add (undefined :: Usermeta)
      ,add (undefined :: VDN)
      ,add (undefined :: VinFormat)
      ,add (undefined :: VipNumber)
      ,add (undefined :: Wazzup)
      ]
