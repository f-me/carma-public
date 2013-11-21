CREATE TABLE "SynCarMake"
  ( id    SERIAL PRIMARY KEY
  , make  int4 REFERENCES "CarMake" ON DELETE SET NULL
  , label text UNIQUE NOT NULL
  );

GRANT ALL ON "SynCarMake" TO carma_db_sync;
GRANT ALL ON "SynCarMake" TO carma_search;
GRANT ALL ON "SynCarMake_id_seq" TO carma_db_sync;
GRANT ALL ON "SynCarMake_id_seq" TO carma_search;
