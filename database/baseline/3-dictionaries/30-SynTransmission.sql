CREATE TABLE "SynTransmission"
  ( id           SERIAL PRIMARY KEY
  , transmission int4 REFERENCES "Transmission"
  , label        text UNIQUE NOT NULL
  );

GRANT ALL ON "SynTransmission" TO carma_db_sync;
GRANT ALL ON "SynTransmission" TO carma_search;
GRANT ALL ON "SynTransmission_id_seq" TO carma_db_sync;
GRANT ALL ON "SynTransmission_id_seq" TO carma_search;

COPY "SynTransmission" (transmission, label) FROM stdin;
1	авт
2	ручн
2	мех
\.
