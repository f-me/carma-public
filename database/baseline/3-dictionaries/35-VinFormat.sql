CREATE TABLE "VinFormat"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL

  , nameLoad                 bool NOT NULL DEFAULT TRUE
  , nameTitles               text[] NOT NULL DEFAULT ARRAY['ФИО клиента']
  , nameDefault              text
  , nameRequired             bool NOT NULL DEFAULT TRUE

  , emailLoad                bool NOT NULL DEFAULT TRUE
  , emailTitle               text NOT NULL DEFAULT 'E-mail клиента'
  , emailDefault             text
  , emailRequired            bool NOT NULL DEFAULT TRUE

  , vinLoad                  bool NOT NULL DEFAULT TRUE
  , vinTitle                 text NOT NULL DEFAULT 'VIN'
  , vinDefault               text
  , vinRequired              bool NOT NULL DEFAULT TRUE

  , cardNumberLoad           bool NOT NULL DEFAULT TRUE
  , cardNumberTitle          text NOT NULL DEFAULT 'Номер карты'
  , cardNumberDefault        text
  , cardNumberRequired       bool NOT NULL DEFAULT TRUE

  , codeWordLoad             bool NOT NULL DEFAULT TRUE
  , codeWordTitle            text NOT NULL DEFAULT 'Кодовое слово'
  , codeWordDefault          text
  , codeWordRequired         bool NOT NULL DEFAULT TRUE

  , phoneLoad                bool NOT NULL DEFAULT TRUE
  , phoneTitle               text NOT NULL DEFAULT 'Номер телефона'
  , phoneDefault             text
  , phoneRequired            bool NOT NULL DEFAULT TRUE

  , plateNumLoad             bool NOT NULL DEFAULT TRUE
  , plateNumTitle            text NOT NULL DEFAULT 'Госномер'
  , plateNumDefault          text
  , plateNumRequired         bool NOT NULL DEFAULT TRUE

  , validSinceLoad           bool NOT NULL DEFAULT TRUE
  , validSinceTitle          text NOT NULL DEFAULT 'Дата регистрации в программе'
  , validSinceFormat         text
  , validSinceDefault        date
  , validSinceRequired       bool NOT NULL DEFAULT TRUE

  , validUntilLoad           bool NOT NULL DEFAULT TRUE
  , validUntilTitle          text NOT NULL DEFAULT 'Программа действует до (Дата)'
  , validUntilFormat         text
  , validUntilDefault        date
  , validUntilRequired       bool NOT NULL DEFAULT TRUE

  , startMileageLoad         bool NOT NULL DEFAULT TRUE
  , startMileageTitle        text NOT NULL DEFAULT 'Пробег при регистрации в программе'
  , startMileageDefault      int4
  , startMileageRequired     bool NOT NULL DEFAULT TRUE

  , makeLoad                 bool NOT NULL DEFAULT TRUE
  , makeTitle                text NOT NULL DEFAULT 'Марка'
  , makeDefault              int4 REFERENCES "CarMake"
  , makeRequired             bool NOT NULL DEFAULT TRUE

  , modelLoad                bool NOT NULL DEFAULT TRUE
  , modelTitle               text NOT NULL DEFAULT 'Модель'
  , modelDefault             int4 REFERENCES "CarModel"
  , modelRequired            bool NOT NULL DEFAULT TRUE

  , makeYearLoad             bool NOT NULL DEFAULT TRUE
  , makeYearTitle            text NOT NULL DEFAULT 'Год производства автомобиля'
  , makeYearDefault          int2
  , makeYearRequired         bool NOT NULL DEFAULT TRUE

  , carClassLoad             bool NOT NULL DEFAULT TRUE
  , carClassTitle            text NOT NULL DEFAULT 'Класс автомобиля'
  , carClassDefault          int4 REFERENCES "CarClass"
  , carClassRequired         bool NOT NULL DEFAULT TRUE

  , colorLoad                bool NOT NULL DEFAULT TRUE
  , colorTitle               text NOT NULL DEFAULT 'Цвет'
  , colorDefault             text
  , colorRequired            bool NOT NULL DEFAULT TRUE

  , transmissionLoad         bool NOT NULL DEFAULT TRUE
  , transmissionTitle        text NOT NULL DEFAULT 'Коробка передач'
  , transmissionDefault      int4 REFERENCES "Transmission"
  , transmissionRequired     bool NOT NULL DEFAULT TRUE

  , engineVolumeLoad         bool NOT NULL DEFAULT TRUE
  , engineVolumeTitle        text NOT NULL DEFAULT 'Объём двигателя'
  , engineVolumeDefault      text
  , engineVolumeRequired     bool NOT NULL DEFAULT TRUE

  , engineTypeLoad           bool NOT NULL DEFAULT TRUE
  , engineTypeTitle          text NOT NULL DEFAULT 'Тип двигателя'
  , engineTypeDefault        int4 REFERENCES "Engine"
  , engineTypeRequired       bool NOT NULL DEFAULT TRUE

  , buyDateLoad              bool NOT NULL DEFAULT TRUE
  , buyDateTitle             text NOT NULL DEFAULT 'Дата покупки'
  , buyDateFormat            text
  , buyDateDefault           date
  , buyDateRequired          bool NOT NULL DEFAULT TRUE

  , sellerLoad               bool NOT NULL DEFAULT TRUE
  , sellerTitle              text NOT NULL DEFAULT 'Дилер, продавший автомобиль'
  , sellerCodeTitle          text NOT NULL DEFAULT 'Дилер, продавший автомобиль (код)'
  , sellerDefault            int4 REFERENCES partnertbl
  , sellerRequired           bool NOT NULL DEFAULT TRUE

  , lastCheckDealerLoad      bool NOT NULL DEFAULT TRUE
  , lastCheckDealerTitle     text NOT NULL DEFAULT 'Дилер, у которого проходило последнее ТО'
  , lastCheckDealerCodeTitle text NOT NULL DEFAULT 'Дилер, у которого проходило последнее ТО (код)'
  , lastCheckDealerDefault   int4 REFERENCES partnertbl
  , lastCheckDealerRequired  bool NOT NULL DEFAULT TRUE

  , lastCheckMileageLoad     bool NOT NULL DEFAULT TRUE
  , lastCheckMileageTitle    text NOT NULL DEFAULT 'Пробег на последнем ТО'
  , lastCheckMileageDefault  int4
  , lastCheckMileageRequired bool NOT NULL DEFAULT TRUE

  , lastCheckDateLoad        bool NOT NULL DEFAULT TRUE
  , lastCheckDateTitle       text NOT NULL DEFAULT 'Дата последнего ТО'
  , lastCheckDateFormat      text
  , lastCheckDateDefault     date
  , lastCheckDateRequired    bool NOT NULL DEFAULT TRUE

  , checkPeriodLoad          bool NOT NULL DEFAULT TRUE
  , checkPeriodTitle         text NOT NULL DEFAULT 'Межсервисный интервал'
  , checkPeriodDefault       int2
  , checkPeriodRequired      bool NOT NULL DEFAULT TRUE

  , checkTypeLoad            bool NOT NULL DEFAULT TRUE
  , checkTypeTitle           text NOT NULL DEFAULT 'Вид ТО'
  , checkTypeDefault         int4 REFERENCES "CheckType"
  , checkTypeRequired        bool NOT NULL DEFAULT TRUE

  , orderNumberLoad          bool NOT NULL DEFAULT TRUE
  , orderNumberTitle         text NOT NULL DEFAULT 'Номер заказ-наряда'
  , orderNumberDefault       text
  , orderNumberRequired      bool NOT NULL DEFAULT TRUE

  , managerNameLoad          bool NOT NULL DEFAULT TRUE
  , managerNameTitles        text[] NOT NULL DEFAULT ARRAY['ФИО менеджера']
  , managerNameDefault       text
  , managerNameRequired      bool NOT NULL DEFAULT TRUE

  , commentLoad              bool NOT NULL DEFAULT TRUE
  , commentTitle             text NOT NULL DEFAULT 'Комментарий'
  , commentDefault           text
  , commentRequired          bool NOT NULL DEFAULT TRUE

  , subprogramLoad           bool NOT NULL DEFAULT TRUE
  , subprogramTitle          text NOT NULL DEFAULT 'Подпрограмма'
  , subprogramRequired       bool NOT NULL DEFAULT TRUE

  , legalFormLoad            bool NOT NULL DEFAULT TRUE
  , legalFormTitle           text NOT NULL DEFAULT 'Физическое/юридическое лицо'
  , legalFormDefault         int4 REFERENCES "LegalForm"
  , legalFormRequired        bool NOT NULL DEFAULT TRUE
  );

GRANT ALL ON "VinFormat" TO carma_db_sync;
GRANT ALL ON "VinFormat" TO carma_search;
GRANT ALL ON "VinFormat_id_seq" TO carma_db_sync;
GRANT ALL ON "VinFormat_id_seq" TO carma_search;
