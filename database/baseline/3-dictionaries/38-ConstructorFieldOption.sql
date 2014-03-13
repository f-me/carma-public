
create table "ConstructorFieldOption" (
  id       SERIAL PRIMARY KEY,
  model    text NOT NULL,
  screen   text,
  program  int4 REFERENCES "Program" ON DELETE CASCADE,
  ord      int4 NOT NULL DEFAULT 0,
  field    text NOT NULL,
  label    text NOT NULL DEFAULT '',
  info     text NOT NULL DEFAULT '',
  required bool NOT NULL DEFAULT false,
  r        bool NOT NULL DEFAULT false,
  w        bool NOT NULL DEFAULT false
);

GRANT ALL ON "ConstructorFieldOption" TO carma_db_sync;
GRANT ALL ON "ConstructorFieldOption" TO carma_search;
GRANT ALL ON "ConstructorFieldOption_id_seq" TO carma_db_sync;
GRANT ALL ON "ConstructorFieldOption_id_seq" TO carma_search;
