module AppHandlers.Backoffice
    (
      -- * Back office analysis
      BORepr(..)
    , serveBackofficeSpec
    )

where

import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Snap

import           Carma.Model
import           Data.Model                  (idents)

import           Carma.Model.ActionResult    (ActionResult)
import           Carma.Model.ActionType      (ActionType)
import           Carma.Model.CaseStatus      (CaseStatus)
import           Carma.Model.FalseCall       (FalseCall)
import           Carma.Model.Program         (Program)
import qualified Carma.Model.Role            as Role
import           Carma.Model.Satisfaction    (Satisfaction)
import           Carma.Model.ServiceStatus   (ServiceStatus)
import           Carma.Model.ServiceType     (ServiceType)
import           Carma.Model.SmsTemplate     (SmsTemplate)

import           Carma.Backoffice
import           Carma.Backoffice.Graph
import           Carma.Backoffice.Text
import           Carma.Backoffice.Validation

import           AppHandlers.Util
import           Application


data BORepr = Txt | Dot | Check


type IdentMap m = Map.Map (IdentI m) Text


serveBackofficeSpec :: BORepr -> AppHandler ()
serveBackofficeSpec repr =
    case repr of
      Txt -> writeText $ backofficeText carmaBackoffice boxedIMap
      Dot -> writeLazyText $ backofficeDot carmaBackoffice boxedIMap
      Check -> writeJSON $ map show $ checkBackoffice carmaBackoffice boxedIMap
    where
      -- Simple ident mapping
      iMap :: Model m => IdentMap m
      iMap = Map.fromList $ map (\(k, v) -> (v, T.pack k)) $ HM.toList idents

      boxMap :: Model m => IdentMap m -> Map.Map IBox Text
      boxMap = Map.mapKeys IBox
      -- Combine mappings for multiple models into one
      boxedIMap = Map.unions [ boxMap (iMap :: IdentMap ActionResult)
                             , boxMap (iMap :: IdentMap ActionType)
                             , boxMap (iMap :: IdentMap CaseStatus)
                             , boxMap (iMap :: IdentMap FalseCall)
                             , boxMap (iMap :: IdentMap Role.Role)
                             , boxMap (iMap :: IdentMap Satisfaction)
                             , boxMap (iMap :: IdentMap ServiceStatus)
                             , boxMap (iMap :: IdentMap ServiceType)
                             , boxMap (iMap :: IdentMap SmsTemplate)
                             , boxMap (iMap :: IdentMap Program)
                             ]

