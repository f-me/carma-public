CREATE TABLE "ProgramInfo"
  ( id       SERIAL PRIMARY KEY
  , program  int4 REFERENCES "Program" NOT NULL
  , info     text NOT NULL DEFAULT ''
  );

GRANT ALL ON "ProgramInfo" TO carma_db_sync;
GRANT ALL ON "ProgramInfo" TO carma_search;
GRANT ALL ON "ProgramInfo_id_seq" TO carma_db_sync;
GRANT ALL ON "ProgramInfo_id_seq" TO carma_search;
