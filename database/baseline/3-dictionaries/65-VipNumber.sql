CREATE TABLE "VipNumber"
  ( id SERIAL PRIMARY KEY
  , number text UNIQUE NOT NULL CHECK (number <> '')
  );

GRANT ALL ON "VipNumber" TO carma_db_sync;
GRANT ALL ON "VipNumber_id_seq" TO carma_db_sync;
