
module Carma.Model.Contract where

import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime)
import Data.Int
import Data.Text
import Data.Typeable
import Data.Model

import Carma.Model.Types
import Carma.Model.CarMake (CarMake)
import Carma.Model.CarModel (CarModel)


-- FIXME: add maybes
data Contract = Contract
  {carVin            :: F Text             "carVin"            ""
  ,carSeller         :: F Text             "carSeller"         "" -- FK Partner
  ,carMake           :: F (Ident CarMake)  "carMake"           "" -- FIXME: redundant
  ,carModel          :: F (Ident CarModel) "carModel"          ""
  ,carPlateNum       :: F Text             "carPlateNum"       ""
  ,carMakeYear       :: F Int16            "carMakeYear"       ""
  ,carColor          :: F Text             "carColor"          "" -- FIXME dict
  ,carBuyDate        :: F Day              "carBuyDate"        ""
  ,carCheckupDate    :: F Day              "carCheckupDate"    ""
  ,carDealerTO       :: F Text             "carDealerTO"       "" -- FK Partner
  ,carCheckupMilage  :: F Int32            "carCheckupMilage"  ""
  ,carTransmission   :: F Text             "carTransmission"   "" -- FIXME dict
  ,carEngine         :: F Text             "carEngine"         "" -- FIXME dict
  ,carCheckupPeriod  :: F Int16            "carCheckupPeriod"  ""
  ,contractType      :: F Text             "contractType"      "" -- FIXME dict
  ,cardNumber        :: F Text             "cardNumber"        ""
  ,validFromDate     :: F Day              "validFromDate"     "" -- NB contractValidFromDate
  ,validUntilDate    :: F Day              "validUntilDate"    "" -- NB contractValidUntilDate
  ,validUntilMilage  :: F Int32            "validUntilMilage"  ""
  ,milageTO          :: F Int32            "milageTO"          ""
  ,cardOwner         :: F Text             "cardOwner"         ""
  ,manager           :: F Text             "manager"           ""
  ,warrantyFromDate  :: F Day              "warrantyFromDate"  "" -- NB field name
  ,warrantyUntilDate :: F Day              "warrantyUntilDate" "" -- NB filed name
  ,ctime             :: F UTCTime          "ctime"             ""
  ,program           :: F Text             "program"           "" -- fk
  ,techType          :: F Text             "techType"          "" -- fk
  ,orderNumber       :: F Text             "orderNumber"       ""
  ,comment           :: F Text             "comment"           ""
  ,dixi              :: F Bool             "dixi"              ""
  ,isActive          :: F Bool             "isActive"          ""
  ,owner             :: F Text             "owner"             "" -- FIXME: remane, fk user
  }
  deriving Typeable


instance Model Contract where
  type TableName Contract = "Contract"
  modelFields = getModelFields Contract
