CREATE TYPE "EventType" AS ENUM ('Login', 'Logout', 'Create', 'Update', 'AvayaNA');

CREATE TABLE "Event"
  ( id        SERIAL PRIMARY KEY
  , userid    int4 REFERENCES usermetatbl (id)
  , ctime     timestamptz  NOT NULL DEFAULT now()
  , type      "EventType"  NOT NULL
  , modelId   int4 NOT NULL
  , modelName text NOT NULL
  , field     text
  , patch     json
  );

CREATE INDEX ON "Event"(modelId);

GRANT ALL ON "Event" TO carma_db_sync;
GRANT ALL ON "Event_id_seq" TO carma_db_sync;
