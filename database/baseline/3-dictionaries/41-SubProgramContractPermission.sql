CREATE TABLE "SubProgramContractPermission"
  ( id SERIAL PRIMARY KEY
  , parent int4 REFERENCES "SubProgram" NOT NULL
  , contractField  text
  , showTable bool NOT NULL DEFAULT FALSE
  , showForm bool NOT NULL DEFAULT FALSE
  );

GRANT ALL ON "SubProgramContractPermission" TO carma_db_sync;
GRANT ALL ON "SubProgramContractPermission" TO carma_search;
GRANT ALL ON "SubProgramContractPermission_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgramContractPermission_id_seq" TO carma_search;
