CREATE TABLE "Transmission"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , synonyms text[]
  );

GRANT ALL ON "Transmission" TO carma_db_sync;
GRANT ALL ON "Transmission" TO carma_search;
GRANT ALL ON "Transmission_id_seq" TO carma_db_sync;
GRANT ALL ON "Transmission_id_seq" TO carma_search;

COPY "Transmission" (id, label,synonyms) FROM stdin;
1	Автоматическая	{авт,ручн}
2	Механическая	{мех}
3	Роботизированная	\N
\.

SELECT setval(pg_get_serial_sequence('"Transmission"', 'id'), max(id)) from "Transmission";
