
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
import Carma.Model.Dictionary          (Dictionary)
import Carma.Model.CarMake             (CarMake)
import Carma.Model.CarModel            (CarModel)
import Carma.Model.City                (City)
import Carma.Model.FieldPermission     (FieldPermission)
import Carma.Model.NewCaseField        (NewCaseField)
import Carma.Model.DefaultNewCaseField (DefaultNewCaseField)
import Carma.Model.Program             (Program)
import Carma.Model.Region              (Region)
import Carma.Model.Role                (Role)
import Carma.Model.SmsTemplate         (SmsTemplate)
import Carma.Model.Sms                 (Sms)
import Carma.Model.Case                (Case)
import Carma.Model.ProgramInfo         (ProgramInfo)
import Carma.Model.ServiceInfo         (ServiceInfo)

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
      ,add (undefined :: Program)
      ,add (undefined :: Region)
      ,add (undefined :: Role)
      ,add (undefined :: SmsTemplate)
      ,add (undefined :: Sms)
      ,add (undefined :: Case)
      ,add (undefined :: ProgramInfo)
      ,add (undefined :: ServiceInfo)
      ]
