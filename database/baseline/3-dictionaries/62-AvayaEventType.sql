CREATE TABLE "AvayaEventType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "AvayaEventType" (id, label) FROM stdin;
1	Входящий звонок
2	Исходящий звонок
3	Удержание
4	Возобновление
5	Конференция
6	Соединение
7	Завершение звонка
\.

GRANT ALL ON "AvayaEventType" TO carma_db_sync;
GRANT ALL ON "AvayaEventType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"AvayaEventType"', 'id'), max(id)) from "AvayaEventType";
