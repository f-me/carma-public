CREATE TABLE "CarMake"
  ( id    SERIAL PRIMARY KEY
  , value text
  , label text UNIQUE NOT NULL
  , synonyms text[]
  , fdds int4
  );

GRANT ALL ON "CarMake" TO carma_db_sync;
GRANT ALL ON "CarMake" TO carma_search;
GRANT ALL ON "CarMake_id_seq" TO carma_db_sync;
GRANT ALL ON "CarMake_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"CarMake"', 'id'), max(id)) from "CarMake";
