CREATE TABLE "ClientRefusalReason"
  ( id       SERIAL PRIMARY KEY
  , label    text UNIQUE NOT NULL CHECK (label <> '')
  );

GRANT ALL ON "ClientRefusalReason" TO carma_db_sync;
GRANT ALL ON "ClientRefusalReason_id_seq" TO carma_db_sync;

INSERT INTO "ClientRefusalReason" (label) VALUES
    ('Неисправность самоустранилась')
  , ('Не устроило время ожидания')
  , ('Изменились планы')
  , ('Не устроили условия ремонта у дилера')
  , ('Сам нашел эвакуатор/техпомощь')
  , ('Сам решил проблему транспортировки')
;
