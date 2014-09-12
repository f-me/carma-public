CREATE TABLE "ServiceType"
  ( id       SERIAL PRIMARY KEY
  , label    text UNIQUE NOT NULL CHECK (label <> '')
  , icon     text NOT NULL DEFAULT ''
  , fdds     text NOT NULL DEFAULT ''
  , model    int4 NOT NULL REFERENCES "CtrModel"
  );

GRANT ALL ON "ServiceType" TO carma_db_sync;
GRANT ALL ON "ServiceType" TO carma_search;

INSERT INTO "ServiceType" (id, label, icon, fdds, model) VALUES
    (1, 'Техпомощь', 'cog', '55', 14)
  , (2, 'Эвакуация', '', '6G', 17)
  , (3, 'Подменный автомобиль', 'road', '', 11)
  , (4, 'Гостиница', '', '', 12)
  , (5, 'Такси', '', '', 13)
  , (6, 'Трезвый водитель', 'glass', '', 12)
  , (7, 'Транспортировка', '', '', 18)
  , (8, 'Доставка ТС', '', '', 6)
  , (9, 'Доставка запчастей', '', '', 7)
  , (10, 'Юридическая помощь', '', '', 10)
  , (11, 'ТО', '', '', 15)
  , (12, 'Информирование о происшествии', '', '', 9)
  , (13, 'Консультация', '', '', 4)
  , (14, 'Заказ билетов', '', '', 16)
  , (15, 'Продолжение путешествия', '', '', 5)
  , (16, 'Банковская поддержка', '', '', 3)
  , (17, 'Доставка клиента', '', '', 19)
  , (18, 'Аварийный комиссар', '', '', 2)
  ;
