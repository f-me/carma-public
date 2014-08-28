CREATE TABLE "ServiceType"
  ( id       SERIAL PRIMARY KEY
  , label    text UNIQUE NOT NULL CHECK (label <> '')
  , icon     text NOT NULL DEFAULT ''
  , fdds     text NOT NULL DEFAULT ''
  , model    int4 NOT NULL REFERENCES "CtrModel"
  );

GRANT ALL ON "ServiceType" TO carma_db_sync;
GRANT ALL ON "ServiceType" TO carma_search;

INSERT INTO "ServiceType" (label, icon, fdds, model) VALUES
    ('Техпомощь', 'cog', '55', 14)
  , ('Эвакуация', '', '6G', 17)
  , ('Подменный автомобиль', 'road', '', 11)
  , ('Гостиница', '', '', 12)
  , ('Такси', '', '', 13)
  , ('Трезвый водитель', 'glass', '', 12)
  , ('Транспортировка', '', '', 18)
  , ('Доставка ТС', '', '', 6)
  , ('Доставка запчастей', '', '', 7)
  , ('Юридическая помощь', '', '', 10)
  , ('ТО', '', '', 15)
  , ('Информирование о происшествии', '', '', 9)
  , ('Консультация', '', '', 4)
  , ('Заказ билетов', '', '', 16)
  , ('Продолжение путешествия', '', '', 5)
  , ('Банковская поддержка', '', '', 3)
  , ('Аварийный комиссар', '', '', 2);
