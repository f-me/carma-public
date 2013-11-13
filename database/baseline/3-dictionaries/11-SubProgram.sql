CREATE TABLE "SubProgram"
  ( id    SERIAL PRIMARY KEY
  , parent int4 REFERENCES "Program" ON DELETE SET NULL
  , mailAddr text
  , mailPass text
  , contacts text
  , services int4[] DEFAULT array[]::int4[]
  , checkPeriod integer
  , validUntil timestamp with timezone
  , contract text
  , logo text
  , help text
  , dealerHelp text
  );

GRANT ALL ON "SubProgram" TO carma_db_sync;
GRANT ALL ON "SubProgram" TO carma_search;
