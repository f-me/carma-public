CREATE TABLE "Activity"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "Activity" (id, label) FROM stdin;
1	Перевод звонка
2	Конференц-связь
3	Выезд
\.

GRANT ALL ON "Activity" TO carma_db_sync;
GRANT ALL ON "Activity_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"Activity"', 'id'), max(id)) from "Activity";
