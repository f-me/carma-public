CREATE TABLE "VDN"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  , number text UNIQUE NOT NULL CHECK (number <> '')
  , greeting text NOT NULL DEFAULT ''
  , program int4 REFERENCES "Program"
  );

GRANT ALL ON "VDN" TO carma_db_sync;
GRANT ALL ON "VDN" TO carma_search;
GRANT ALL ON "VDN_id_seq" TO carma_db_sync;
GRANT ALL ON "VDN_id_seq" TO carma_search;
