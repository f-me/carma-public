CREATE TABLE "System"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

GRANT ALL ON "System" TO carma_db_sync;
GRANT ALL ON "System_id_seq" TO carma_db_sync;
