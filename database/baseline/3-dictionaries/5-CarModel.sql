CREATE TABLE "CarModel"
  ( id     SERIAL PRIMARY KEY
  , value  text
  , label  text NOT NULL
  , parent int4 REFERENCES "CarMake" NOT NULL ON DELETE SET NULL
  , info   text NOT NULL DEFAULT ''
  , synonyms text[]
  , fdds int4
  ,UNIQUE (label, parent)
  );

CREATE UNIQUE INDEX ON "CarModel" (label) WHERE parent IS NULL;

GRANT ALL ON "CarModel" TO carma_search;
GRANT ALL ON "CarModel" TO carma_db_sync;
GRANT ALL ON "CarModel_id_seq" TO carma_db_sync;
GRANT ALL ON "CarModel_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"CarModel"', 'id'), max(id)) from "CarModel";
