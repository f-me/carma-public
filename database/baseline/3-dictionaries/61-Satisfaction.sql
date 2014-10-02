CREATE TABLE "Satisfaction"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "Satisfaction" (id, label) FROM stdin;
1	Клиент доволен
2	Клиент не доволен
3	Клиент не отвечает
\.

GRANT ALL ON "Satisfaction" TO carma_db_sync;
GRANT ALL ON "Satisfaction" TO carma_search;
GRANT ALL ON "Satisfaction_id_seq" TO carma_db_sync;
GRANT ALL ON "Satisfaction_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"Satisfaction"', 'id'), max(id)) from "Satisfaction";
