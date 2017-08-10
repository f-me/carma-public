CREATE TABLE "CarGeneration"
  ( id     SERIAL PRIMARY KEY
  , label  text NOT NULL
  , parent int4 REFERENCES "CarModel" NOT NULL
  , synonyms text[]
  , UNIQUE (label)
  );

CREATE UNIQUE INDEX ON "CarGeneration" (label);

GRANT ALL ON "CarGeneration" TO carma_db_sync;
GRANT ALL ON "CarGeneration_id_seq" TO carma_db_sync;

GRANT SELECT ON "CarGeneration" TO reportgen;

SELECT setval(pg_get_serial_sequence('"CarGeneration"', 'id'), max(id)) from "CarGeneration";
