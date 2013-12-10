CREATE TABLE "Transmission"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  );

GRANT ALL ON "Transmission" TO carma_db_sync;
GRANT ALL ON "Transmission" TO carma_search;
GRANT ALL ON "Transmission_id_seq" TO carma_db_sync;
GRANT ALL ON "Transmission_id_seq" TO carma_search;

COPY "Transmission" (id, label) FROM stdin;
1	Автоматическая
2	Механическая
3	Роботизированная
\.
