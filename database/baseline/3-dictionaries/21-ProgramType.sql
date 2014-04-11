CREATE TABLE "ProgramType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  );

COPY "ProgramType" (id, label) FROM stdin;
1	B2B
2	B2C
\.

GRANT ALL ON "ProgramType" TO carma_db_sync;
GRANT ALL ON "ProgramType" TO carma_search;
GRANT ALL ON "ProgramType_id_seq" TO carma_db_sync;
GRANT ALL ON "ProgramType_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"ProgramType"', 'id'), max(id)) from "ProgramType";
