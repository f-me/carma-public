CREATE TABLE "SmsTokenName"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL
  , var_name text UNIQUE NOT NULL
  );

GRANT ALL ON "SmsTokenName" TO carma_db_sync;
GRANT ALL ON "SmsTokenName" TO carma_search;
GRANT ALL ON "SmsTokenName_id_seq" TO carma_db_sync;
GRANT ALL ON "SmsTokenName_id_seq" TO carma_search;
