
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Carma.Model
  (Model
  ,Ident(..), IdentI, IdentT
  ,dispatch
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Typeable

import Data.Model
import Carma.Model.Dictionary             (Dictionary)
import Carma.Model.CarMake                (CarMake)
import Carma.Model.CarModel               (CarModel)
import Carma.Model.City                   (City)
import Carma.Model.FieldPermission        (FieldPermission)
import Carma.Model.NewCaseField           (NewCaseField)
import Carma.Model.DefaultNewCaseField    (DefaultNewCaseField)
import Carma.Model.ConstructorFieldOption (ConstructorFieldOption)
import Carma.Model.Program                (Program)
import Carma.Model.Region                 (Region)
import Carma.Model.Role                   (Role)
import Carma.Model.SmsTemplate            (SmsTemplate)
import Carma.Model.Sms                    (Sms)
import Carma.Model.Case                   (Case)
import Carma.Model.ProgramInfo            (ProgramInfo)
import Carma.Model.ServiceInfo            (ServiceInfo)
import Carma.Model.ServiceNames           (ServiceNames)
import Carma.Model.Service                (Service)
import Carma.Model.Service.AverageCommissioner (AverageCommissioner)
import Carma.Model.Service.Bank           (Bank)
import Carma.Model.Service.Consultation   (Consultation)
import Carma.Model.Service.Continue       (Continue)
import Carma.Model.Service.DeliverCar     (DeliverCar)
import Carma.Model.Service.DeliverClient  (DeliverClient)
import Carma.Model.Service.DeliverParts   (DeliverParts)
import Carma.Model.Service.Hotel          (Hotel)
import Carma.Model.Service.Information    (Information)
import Carma.Model.Service.Insurance      (Insurance)
import Carma.Model.Service.LegalAssistance(LegalAssistance)
import Carma.Model.Service.Rent           (Rent)
import Carma.Model.Service.SoberDriver    (SoberDriver)
import Carma.Model.Service.Taxi           (Taxi)
import Carma.Model.Service.Tickets        (Tickets)
import Carma.Model.Service.Transportation (Transportation)
import Carma.Model.Service.Tech           (Tech)
import Carma.Model.Service.TechInspect    (TechInspect)
import Carma.Model.Service.Towage         (Towage)

dispatch :: forall a . Text -> (forall m . Model m => m -> a) -> Maybe a
dispatch model fn = Map.lookup model modelMap
  where
    add :: Model m => m -> (Text, a)
    add m = (T.pack $ show $ typeOf m, fn m)
    modelMap = Map.fromList
      [add (undefined :: Dictionary)
      ,add (undefined :: CarMake)
      ,add (undefined :: CarModel)
      ,add (undefined :: City)
      ,add (undefined :: FieldPermission)
      ,add (undefined :: NewCaseField)
      ,add (undefined :: DefaultNewCaseField)
      ,add (undefined :: ConstructorFieldOption)
      ,add (undefined :: Program)
      ,add (undefined :: Region)
      ,add (undefined :: Role)
      ,add (undefined :: SmsTemplate)
      ,add (undefined :: Sms)
      ,add (undefined :: Case)
      ,add (undefined :: ProgramInfo)
      ,add (undefined :: ServiceInfo)
      ,add (undefined :: ServiceNames)
      ,add (undefined :: Service)
      ,add (undefined :: AverageCommissioner)
      ,add (undefined :: Bank)
      ,add (undefined :: Consultation)
      ,add (undefined :: Continue)
      ,add (undefined :: DeliverCar)
      ,add (undefined :: DeliverClient)
      ,add (undefined :: DeliverParts)
      ,add (undefined :: Hotel)
      ,add (undefined :: Information)
      ,add (undefined :: Insurance)
      ,add (undefined :: LegalAssistance)
      ,add (undefined :: Rent)
      ,add (undefined :: SoberDriver)
      ,add (undefined :: Taxi)
      ,add (undefined :: Tickets)
      ,add (undefined :: Transportation)
      ,add (undefined :: Tech)
      ,add (undefined :: TechInspect)
      ,add (undefined :: Towage)
      ]
