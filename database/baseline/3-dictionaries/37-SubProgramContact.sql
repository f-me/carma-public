CREATE TABLE "SubProgramContact"
  ( id    SERIAL PRIMARY KEY
  , parent int4 REFERENCES "SubProgram" NOT NULL
  , name text
  , email text
  , phone text
  );

GRANT ALL ON "SubProgramContact" TO carma_db_sync;
GRANT ALL ON "SubProgramContact" TO carma_search;
GRANT ALL ON "SubProgramContact_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgramContact_id_seq" TO carma_search;
