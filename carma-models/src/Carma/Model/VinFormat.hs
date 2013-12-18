module Carma.Model.VinFormat where

import Data.Time.Calendar (Day)
import Data.Text
import Data.Typeable
import Data.Vector

import Data.Model
import Data.Model.View

import Carma.Model.Types (TInt)

import Carma.Model.CarClass     (CarClass)
import Carma.Model.CarMake      (CarMake)
import Carma.Model.CarModel     (CarModel)
import Carma.Model.CheckType    (CheckType)
import Carma.Model.Colors       (Colors)
import Carma.Model.LegalForm    (LegalForm)
import Carma.Model.Partner      (Partner)
import Carma.Model.Transmission (Transmission)
import Carma.Model.Engine       (Engine)


-- TODO: Generate this using TH
data VinFormat = VinFormat
  { ident                    :: PK Int VinFormat "Формат VIN"
  , label                    :: F Text "label" "Название формата"

  , nameLoad                 :: F Bool "nameLoad" "Загружать поле «ФИО клиента»"
  , nameTitles               :: F (Maybe (Vector Text)) "nameTitles" "Заголовки полей с ФИО для «ФИО клиента»"
  , nameDefault              :: F (Maybe Text) "nameDefault" "Значение поля «ФИО клиента» по умолчанию"
  , nameRequired             :: F Bool "nameRequired" "Поле «ФИО клиента» обязательно"

  , emailLoad                :: F Bool "emailLoad" "Загружать поле «E-mail клиента»"
  , emailTitle               :: F Text "emailTitle" "Заголовок поля «E-mail клиента»"
  , emailDefault             :: F (Maybe Text) "emailDefault" "Значение поля «E-mail клиента» по умолчанию"
  , emailRequired            :: F Bool "emailRequired" "Поле «E-mail клиента» обязательно"

  , vinLoad                  :: F Bool "vinLoad" "Загружать поле «VIN»"
  , vinTitle                 :: F Text "vinTitle" "Заголовок поля «VIN»"
  , vinDefault               :: F (Maybe Text) "vinDefault" "Значение поля «VIN» по умолчанию"
  , vinRequired              :: F Bool "vinRequired" "Поле «VIN» обязательно"

  , cardNumberLoad           :: F Bool "cardNumberLoad" "Загружать поле «Номер карты»"
  , cardNumberTitle          :: F Text "cardNumberTitle" "Заголовок поля «Номер карты»"
  , cardNumberDefault        :: F (Maybe Text) "cardNumberDefault" "Значение поля «Номер карты» по умолчанию"
  , cardNumberRequired       :: F Bool "cardNumberRequired" "Поле «Номер карты» обязательно"

  , codeWordLoad             :: F Bool "codeWordLoad" "Загружать поле «Кодовое слово»"
  , codeWordTitle            :: F Text "codeWordTitle" "Заголовок поля «Кодовое слово»"
  , codeWordDefault          :: F (Maybe Text) "codeWordDefault" "Значение поля «Кодовое слово» по умолчанию"
  , codeWordRequired         :: F Bool "codeWordRequired" "Поле «Кодовое слово» обязательно"

  , phoneLoad                :: F Bool "phoneLoad" "Загружать поле «Номер телефона»"
  , phoneTitle               :: F Text "phoneTitle" "Заголовок поля «Номер телефона»"
  , phoneDefault             :: F (Maybe Text) "phoneDefault" "Значение поля «Номер телефона» по умолчанию"
  , phoneRequired            :: F Bool "phoneRequired" "Поле «Номер телефона» обязательно"

  , plateNumLoad             :: F Bool "plateNumLoad" "Загружать поле «Госномер»"
  , plateNumTitle            :: F Text "plateNumTitle" "Заголовок поля «Госномер»"
  , plateNumDefault          :: F (Maybe Text) "plateNumDefault" "Значение поля «Госномер» по умолчанию"
  , plateNumRequired         :: F Bool "plateNumRequired" "Поле «Госномер» обязательно"

  , validSinceLoad           :: F Bool "validSinceLoad" "Загружать поле «Дата регистрации в программе»"
  , validSinceTitle          :: F Text "validSinceTitle" "Заголовок поля «Дата регистрации в программе»"
  , validSinceFormat         :: F (Maybe Text) "validSinceFormat" "Формат поля «Дата регистрации в программе»"
  , validSinceDefault        :: F (Maybe Day) "validSinceDefault" "Значение поля «Дата регистрации в программе» по умолчанию"
  , validSinceRequired       :: F Bool "validSinceRequired" "Поле «Дата регистрации в программе» обязательно"

  , validUntilLoad           :: F Bool "validUntilLoad" "Загружать поле «Программа действует до (Дата)»"
  , validUntilTitle          :: F Text "validUntilTitle" "Заголовок поля «Программа действует до (Дата)»"
  , validUntilFormat         :: F (Maybe Text) "validUntilFormat" "Формат поля «Программа действует до (Дата)»"
  , validUntilDefault        :: F (Maybe Day) "validUntilDefault" "Значение поля «Программа действует до (Дата)» по умолчанию"
  , validUntilRequired       :: F Bool "validUntilRequired" "Поле «Программа действует до (Дата)» обязательно"

  , startMileageLoad         :: F Bool "startMileageLoad" "Загружать поле «Пробег при регистрации в программе»"
  , startMileageTitle        :: F Text "startMileageTitle" "Заголовок поля «Пробег при регистрации в программе»"
  , startMileageDefault      :: F (Maybe TInt) "startMileageDefault" "Значение поля «Пробег при регистрации в программе» по умолчанию"
  , startMileageRequired     :: F Bool "startMileageRequired" "Поле «Пробег при регистрации в программе» обязательно"

  , makeLoad                 :: F Bool "makeLoad" "Загружать поле «Марка»"
  , makeTitle                :: F Text "makeTitle" "Заголовок поля «Марка»"
  , makeDefault              :: F (Maybe (IdentI CarMake)) "makeDefault" "Значение поля «Марка» по умолчанию"
  , makeRequired             :: F Bool "makeRequired" "Поле «Марка» обязательно"

  , modelLoad                :: F Bool "modelLoad" "Загружать поле «Модель»"
  , modelTitle               :: F Text "modelTitle" "Заголовок поля «Модель»"
  , modelDefault             :: F (Maybe (IdentI CarModel)) "modelDefault" "Значение поля «Модель» по умолчанию"
  , modelRequired            :: F Bool "modelRequired" "Поле «Модель» обязательно"

  , makeYearLoad             :: F Bool "makeYearLoad" "Загружать поле «Год производства автомобиля»"
  , makeYearTitle            :: F Text "makeYearTitle" "Заголовок поля «Год производства автомобиля»"
  , makeYearDefault          :: F (Maybe TInt) "makeYearDefault" "Значение поля «Год производства автомобиля» по умолчанию"
  , makeYearRequired         :: F Bool "makeYearRequired" "Поле «Год производства автомобиля» обязательно"

  , carClassLoad             :: F Bool "carClassLoad" "Загружать поле «Класс автомобиля»"
  , carClassTitle            :: F Text "carClassTitle" "Заголовок поля «Класс автомобиля»"
  -- TODO New Year (pun intended) field type
  , carClassDefault          :: F (Maybe (IdentI CarClass)) "carClassDefault" "Значение поля «Класс автомобиля» по умолчанию"
  , carClassRequired         :: F Bool "carClassRequired" "Поле «Класс автомобиля» обязательно"

  , colorLoad                :: F Bool "colorLoad" "Загружать поле «Цвет»"
  , colorTitle               :: F Text "colorTitle" "Заголовок поля «Цвет»"
  , colorDefault             :: F (Maybe (IdentI Colors)) "colorDefault" "Значение поля «Цвет» по умолчанию"
  , colorRequired            :: F Bool "colorRequired" "Поле «Цвет» обязательно"

  , transmissionLoad         :: F Bool "transmissionLoad" "Загружать поле «Коробка передач»"
  , transmissionTitle        :: F Text "transmissionTitle" "Заголовок поля «Коробка передач»"
  , transmissionDefault      :: F (Maybe (IdentI Transmission)) "transmissionDefault" "Значение поля «Коробка передач» по умолчанию"
  , transmissionRequired     :: F Bool "transmissionRequired" "Поле «Коробка передач» обязательно"

  , engineVolumeLoad         :: F Bool "engineVolumeLoad" "Загружать поле «Объём двигателя»"
  , engineVolumeTitle        :: F Text "engineVolumeTitle" "Заголовок поля «Объём двигателя»"
  , engineVolumeDefault      :: F (Maybe Double) "engineVolumeDefault" "Значение поля «Объём двигателя» по умолчанию"
  , engineVolumeRequired     :: F Bool "engineVolumeRequired" "Поле «Объём двигателя» обязательно"

  , engineTypeLoad           :: F Bool "engineTypeLoad" "Загружать поле «Тип двигателя»"
  , engineTypeTitle          :: F Text "engineTypeTitle" "Заголовок поля «Тип двигателя»"
  , engineTypeDefault        :: F (Maybe (IdentI Engine)) "engineTypeDefault" "Значение поля «Тип двигателя» по умолчанию"
  , engineTypeRequired       :: F Bool "engineTypeRequired" "Поле «Тип двигателя» обязательно"

  , buyDateLoad              :: F Bool "buyDateLoad" "Загружать поле «Дата покупки»"
  , buyDateTitle             :: F Text "buyDateTitle" "Заголовок поля «Дата покупки»"
  , buyDateFormat            :: F (Maybe Text) "buyDateFormat" "Формат поля «Дата покупки»"
  , buyDateDefault           :: F (Maybe Day) "buyDateDefault" "Значение поля «Дата покупки» по умолчанию"
  , buyDateRequired          :: F Bool "buyDateRequired" "Поле «Дата покупки» обязательно"

  , sellerLoad               :: F Bool "sellerLoad" "Загружать поле «Дилер, продавший автомобиль»"
  , sellerTitle              :: F Text "sellerTitle" "Заголовок названия дилера для поля «Дилер, продавший автомобиль»"
  , sellerCodeTitle          :: F Text "sellerCodeTitle" "Заголовок кода дилера для поля «Дилер, продавший автомобиль»"
  , sellerDefault            :: F (Maybe (IdentI Partner)) "sellerDefault" "Значение поля «Дилер, продавший автомобиль» по умолчанию"
  , sellerRequired           :: F Bool "sellerRequired" "Поле «Дилер, продавший автомобиль» обязательно"

  , lastCheckDealerLoad      :: F Bool "lastCheckDealerLoad" "Загружать поле «Дилер, у которого проходило последнее ТО»"
  , lastCheckDealerTitle     :: F Text "lastCheckDealerTitle" "Заголовок названия дилера для поля «Дилер, у которого проходило последнее ТО»"
  , lastCheckDealerCodeTitle :: F Text "lastCheckDealerCodeTitle" "Заголовок кода дилера для поля «Дилер, продавший автомобиль»"
  , lastCheckDealerDefault   :: F (Maybe (IdentI Partner)) "lastCheckDealerDefault" "Значение поля «Дилер, у которого проходило последнее ТО» по умолчанию"
  , lastCheckDealerRequired  :: F Bool "lastCheckDealerRequired" "Поле «Дилер, у которого проходило последнее ТО» обязательно"

  , lastCheckMileageLoad     :: F Bool "lastCheckMileageLoad" "Загружать поле «Пробег на последнем ТОLoad»"
  , lastCheckMileageTitle    :: F Text "lastCheckMileageTitle" "Загружать поле «Пробег на последнем ТОTitle»"
  , lastCheckMileageDefault  :: F (Maybe TInt) "lastCheckMileageDefault" "Значение поля «Пробег на последнем ТОTitle» по умолчанию"
  , lastCheckMileageRequired :: F Bool "lastCheckMileageRequired" "Поле «Пробег на последнем ТОTitle» обязательно"

  , lastCheckDateLoad        :: F Bool "lastCheckDateLoad" "Загружать поле «Дата последнего ТО»"
  , lastCheckDateTitle       :: F Text "lastCheckDateTitle" "Заголовок поля «Дата последнего ТО»"
  , lastCheckDateFormat      :: F (Maybe Text) "lastCheckDateFormat" "Формат поля «Дата последнего ТО»"
  , lastCheckDateDefault     :: F (Maybe Day) "lastCheckDateDefault" "Значение поля «Дата последнего ТО» по умолчанию"
  , lastCheckDateRequired    :: F Bool "lastCheckDateRequired" "Поле «Дата последнего ТО» обязательно"

  , checkPeriodLoad          :: F Bool "checkPeriodLoad" "Загружать поле «Межсервисный интервал»"
  , checkPeriodTitle         :: F Text "checkPeriodTitle" "Заголовок поля «Межсервисный интервал»"
  , checkPeriodDefault       :: F (Maybe TInt) "checkPeriodDefault" "Значение поля «Межсервисный интервал» по умолчанию"
  , checkPeriodRequired      :: F Bool "checkPeriodRequired" "Поле «Межсервисный интервал» обязательно"

  , checkTypeLoad            :: F Bool "checkTypeLoad" "Загружать поле «Вид ТО»"
  , checkTypeTitle           :: F Text "checkTypeTitle" "Заголовок поля «Вид ТО»"
  , checkTypeDefault         :: F (Maybe (IdentI CheckType)) "checkTypeDefault" "Значение поля «Вид ТО» по умолчанию"
  , checkTypeRequired        :: F Bool "checkTypeRequired" "Поле «Вид ТО» обязательно"

  , orderNumberLoad          :: F Bool "orderNumberLoad" "Загружать поле «Номер заказ-наряда»"
  , orderNumberTitle         :: F Text "orderNumberTitle" "Заголовок поля «Номер заказ-наряда»"
  , orderNumberDefault       :: F (Maybe Text) "orderNumberDefault" "Значение поля «Номер заказ-наряда» по умолчанию"
  , orderNumberRequired      :: F Bool "orderNumberRequired" "Поле «Номер заказ-наряда» обязательно"

  , managerNameLoad          :: F Bool "managerNameLoad" "Загружать поле «ФИО менеджера»"
  , managerNameTitles        :: F (Maybe (Vector Text)) "managerNameTitles" "Заголовки полей с ФИО для «ФИО менеджера»"
  , managerNameDefault       :: F (Maybe Text) "managerNameDefault" "Значение поля «ФИО менеджера» по умолчанию"
  , managerNameRequired      :: F Bool "managerNameRequired" "Поле «ФИО менеджера» обязательно"

  , commentLoad              :: F Bool "commentLoad" "Загружать поле «Комментарий»"
  , commentTitle             :: F Text "commentTitle" "Заголовок поля «Комментарий»"
  , commentDefault           :: F (Maybe Text) "commentDefault" "Значение поля «Комментарий» по умолчанию"
  , commentRequired          :: F Bool "commentRequired" "Поле «Комментарий» обязательно"

  , cardTypeLoad             :: F Bool "cardTypeLoad" "Загружать поле «Тип карты»"
  , cardTypeTitle            :: F Text "cardTypeTitle" "Заголовок поля «Тип карты»"
  , cardTypeRequired         :: F Bool "cardTypeRequired" "Поле «Тип карты» обязательно"

  , legalFormLoad            :: F Bool "legalFormLoad" "Загружать поле «Физическое/юридическое лицо»"
  , legalFormTitle           :: F Text "legalFormTitle" "Заголовок поля «Физическое/юридическое лицо»"
  , legalFormDefault         :: F (Maybe (IdentI LegalForm)) "legalFormDefault" "Значение поля «Физическое/юридическое лицо» по умолчанию"
  , legalFormRequired        :: F Bool "legalFormRequired" "Поле «Физическое/юридическое лицо» обязательно"
  } deriving Typeable


instance Model VinFormat where
  type TableName VinFormat = "VinFormat"
  modelInfo = mkModelInfo VinFormat ident
  modelView _ = modifyView defaultView
                [ setMeta "dictionaryParent" "makeDefault" modelDefault
                ]
