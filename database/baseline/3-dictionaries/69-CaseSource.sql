CREATE TABLE "CaseSource"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "CaseSource" (id, label) FROM stdin;
1	Оператор
2	Мобильное приложение
3	ДТП
\.

GRANT ALL ON "CaseSource" TO carma_db_sync;
GRANT ALL ON "CaseSource_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"CaseSource"', 'id'), max(id)) from "CaseSource";
