CREATE TABLE "Suggestion"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , fdds text
  );

GRANT ALL ON "Suggestion" TO carma_db_sync;
GRANT ALL ON "Suggestion" TO carma_search;
GRANT ALL ON "Suggestion_id_seq" TO carma_db_sync;
GRANT ALL ON "Suggestion_id_seq" TO carma_search;
