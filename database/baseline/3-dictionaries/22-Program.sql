CREATE TABLE "Program"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , client text
  , clientAddress text
  , clientCode text
  );

GRANT ALL ON "Program" TO carma_db_sync;
GRANT ALL ON "Program" TO carma_search;
GRANT ALL ON "Program_id_seq" TO carma_db_sync;
GRANT ALL ON "Program_id_seq" TO carma_search;
