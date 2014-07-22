CREATE TABLE "SubProgramService"
  ( id    SERIAL PRIMARY KEY
  , parent int4 REFERENCES "SubProgram" NOT NULL
  , type int4 REFERENCES "ServiceType"
  , maxCost text
  , maxDistance integer
  , maxPeriod integer
  , maxCount integer
  );

GRANT ALL ON "SubProgramService" TO carma_db_sync;
GRANT ALL ON "SubProgramService" TO carma_search;
GRANT ALL ON "SubProgramService_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgramService_id_seq" TO carma_search;
