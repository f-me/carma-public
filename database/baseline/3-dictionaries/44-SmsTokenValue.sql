CREATE TABLE "SmsTokenValue"
  ( id    SERIAL PRIMARY KEY
  , token int4 REFERENCES "SmsTokenValue" NOT NULL
  , program int4 REFERENCES "Program" NOT NULL
  , sub_program int4 REFERENCES "SubProgram" NOT NULL
  , value text
  );

GRANT ALL ON "SmsTokenValue" TO carma_db_sync;
GRANT ALL ON "SmsTokenValue" TO carma_search;
GRANT ALL ON "SmsTokenValue_id_seq" TO carma_db_sync;
GRANT ALL ON "SmsTokenValue_id_seq" TO carma_search;
