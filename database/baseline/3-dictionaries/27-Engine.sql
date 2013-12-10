CREATE TABLE "Engine"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  );

GRANT ALL ON "Engine" TO carma_db_sync;
GRANT ALL ON "Engine" TO carma_search;
GRANT ALL ON "Engine_id_seq" TO carma_db_sync;
GRANT ALL ON "Engine_id_seq" TO carma_search;

COPY "Engine" (id, label) FROM stdin;
1	Бензин
2	Дизель
\.
