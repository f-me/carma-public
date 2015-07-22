CREATE TABLE "RequestType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "RequestType" (id, label) FROM stdin;
1	Консультация
2	Первичный осмотр
3	Оценка
4	Отчёт
\.

GRANT ALL ON "RequestType" TO carma_db_sync;
GRANT ALL ON "RequestType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"RequestType"', 'id'), max(id)) from "RequestType";
