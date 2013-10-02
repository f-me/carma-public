
CREATE TABLE "Region"
  (id     SERIAL PRIMARY KEY
  ,label  text UNIQUE NOT NULL
  ,cities int4[] DEFAULT array[]::int4[] -- ELEMENT REFERENCES "City"
  );

GRANT ALL ON "Region" TO carma_db_sync;
GRANT ALL ON "Region" TO carma_search;
GRANT ALL ON "Region_id_seq" TO carma_db_sync;
GRANT ALL ON "Region_id_seq" TO carma_search;
