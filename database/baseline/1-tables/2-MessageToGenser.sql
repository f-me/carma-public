
CREATE TABLE "MessageToGenser"
  (id        SERIAL PRIMARY KEY
  ,ctime     timestamptz NOT NULL DEFAULT now()
  ,mtime     timestamptz NOT NULL DEFAULT now()
  ,status    text NOT NULL DEFAUlT 'draft'
  ,email     text
  ,msgData   json NOT NULL
  );

GRANT ALL ON "MessageToGenser" TO carma_db_sync;
GRANT ALL ON "MessageToGenser_id_seq" TO carma_db_sync;
GRANT ALL ON "MessageToGenser" TO carma_genser_svc;

