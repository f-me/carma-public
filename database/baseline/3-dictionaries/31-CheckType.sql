CREATE TABLE "CheckType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  );

GRANT ALL ON "CheckType" TO carma_db_sync;
GRANT ALL ON "CheckType" TO carma_search;
GRANT ALL ON "CheckType_id_seq" TO carma_db_sync;
GRANT ALL ON "CheckType_id_seq" TO carma_search;

COPY "CheckType" (id, label) FROM stdin;
1	Масляный сервис
2	Интервальный сервис
3	Инспекционный сервис
\.
