CREATE TABLE "AvayaEvent"
  ( id SERIAL PRIMARY KEY
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , eType int4 REFERENCES "AvayaEventType" NOT NULL
  , operator int4 REFERENCES usermetatbl (id) NOT NULL
  , currentAction int4 REFERENCES actiontbl NOT NULL
  , interlocutor text
  , callId text
  );

GRANT ALL ON "AvayaEvent" TO carma_db_sync;
GRANT ALL ON "AvayaEvent" TO carma_search;
GRANT ALL ON "AvayaEvent_id_seq" TO carma_db_sync;
GRANT ALL ON "AvayaEvent_id_seq" TO carma_search;
