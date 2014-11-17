CREATE TABLE "FalseCall"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "FalseCall" (id, label) FROM stdin;
1	Нет
2	С выставлением счёта
3	Без выставления счёта
\.

GRANT ALL ON "FalseCall" TO carma_db_sync;
GRANT ALL ON "FalseCall" TO carma_search;
GRANT ALL ON "FalseCall_id_seq" TO carma_db_sync;
GRANT ALL ON "FalseCall_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"FalseCall"', 'id'), max(id)) from "FalseCall";
