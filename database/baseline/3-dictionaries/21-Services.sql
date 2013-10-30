
CREATE TABLE "Services"
  (
    id       SERIAL PRIMARY KEY
  , label    text NOT NULL DEFAULT ''
  , value    text UNIQUE NOT NULL
  , icon     text DEFAULT ''
  );

GRANT ALL ON "Services" TO carma_db_sync;
GRANT ALL ON "Services" TO carma_search;

INSERT INTO "Services" (value, label, icon) VALUES
    ('tech', 'Техпомощь', 'cog')
  , ('towage', 'Эвакуация', '')
  , ('rent', 'Подменный автомобиль', 'road')
  , ('hotel', 'Гостиница', '')
  , ('taxi', 'Такси', '')
  , ('sober', 'Трезвый водитель', 'glass')
  , ('transportation', 'Транспортировка', '')
  , ('deliverCar', 'Доставка ТС', '')
  , ('deliverParts', 'Доставка запчастей', '')
  , ('ken', 'Юридическая помощь', '')
  , ('tech1', 'ТО', '')
  , ('information', 'Информирование о происшествии', '')
  , ('consultation', 'Консультация', '')
  , ('tickets', 'Заказ билетов', '')
  , ('continue', 'Продолжение путешествия', '')
  , ('bank', 'Банковская поддержка', '')
  , ('deliverClient', 'Доставка клиента к отремонтированному автомобилю', '')
  , ('averageCommissioner', 'Аварийный комиссар', '')
  , ('insurance', 'Сбор справок для страховой компании', '')

