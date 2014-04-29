CREATE TABLE "System"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , fdds text
  );

GRANT ALL ON "System" TO carma_db_sync;
GRANT ALL ON "System" TO carma_search;
GRANT ALL ON "System_id_seq" TO carma_db_sync;
GRANT ALL ON "System_id_seq" TO carma_search;
