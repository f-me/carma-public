CREATE TABLE "Program"
  ( id    SERIAL PRIMARY KEY
  , value text UNIQUE NOT NULL
  , label text UNIQUE NOT NULL
  , active bool NOT NULL DEFAULT true
  , client text
  , clientCode text
  , clientAddress text
  );

GRANT ALL ON "Program" TO carma_db_sync;
GRANT ALL ON "Program" TO carma_search;
