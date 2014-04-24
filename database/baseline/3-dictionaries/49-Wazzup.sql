CREATE TABLE "Wazzup"
  ( id SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , system int4 REFERENCES "System"
  , part int4 REFERENCES "Part"
  , cause int4 REFERENCES "Cause"
  , suggestion int4 REFERENCES "Suggestion"
  );

GRANT ALL ON "Wazzup" TO carma_db_sync;
GRANT ALL ON "Wazzup" TO carma_search;
GRANT ALL ON "Wazzup_id_seq" TO carma_db_sync;
GRANT ALL ON "Wazzup_id_seq" TO carma_search;
