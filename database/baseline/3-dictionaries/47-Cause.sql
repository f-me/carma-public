CREATE TABLE "Cause"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , fdds int4
  );

GRANT ALL ON "Cause" TO carma_db_sync;
GRANT ALL ON "Cause" TO carma_search;
GRANT ALL ON "Cause_id_seq" TO carma_db_sync;
GRANT ALL ON "Cause_id_seq" TO carma_search;
