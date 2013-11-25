CREATE TABLE "SubProgram"
  ( id    SERIAL PRIMARY KEY
  , parent int4 REFERENCES "Program" ON DELETE SET NULL
  , label text NOT NULL
  , active bool NOT NULL DEFAULT true
  , value text UNIQUE NOT NULL
  , mailAddr text
  , mailPass text
  , contacts text
  , services int4[] DEFAULT array[]::int4[]
  , checkPeriod integer
  , validUntil integer
  , contract text
  , logo text
  , help text
  , dealerHelp text
  );

GRANT ALL ON "SubProgram" TO carma_db_sync;
GRANT ALL ON "SubProgram" TO carma_search;
GRANT ALL ON "SubProgram_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgram_id_seq" TO carma_search;
