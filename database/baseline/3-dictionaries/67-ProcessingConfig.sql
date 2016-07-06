CREATE TABLE "ProcessingConfig"
  ( id    SERIAL PRIMARY KEY
  , actionsFirst bool NOT NULL DEFAULT 't'
  , afterCallSeconds int4 NOT NULL DEFAULT 30
  , callWaitSeconds int4 NOT NULL DEFAULT 30
  , avgSuburbanSpeed numeric(5,2)  NOT NULL DEFAULT 60
  );

COPY "ProcessingConfig" (id, actionsFirst, afterCallSeconds, callWaitSeconds) FROM stdin;
1	t	30	30	60
\.

GRANT ALL ON "ProcessingConfig" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"ProcessingConfig"', 'id'), max(id)) from "ProcessingConfig";
