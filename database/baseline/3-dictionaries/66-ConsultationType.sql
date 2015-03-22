CREATE TABLE "ConsultationType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "ConsultationType" (id, label) FROM stdin;
1	Консультация оператора
2	Консультация механика
\.

GRANT ALL ON "ConsultationType" TO carma_db_sync;
GRANT ALL ON "ConsultationType" TO carma_search;
GRANT ALL ON "ConsultationType_id_seq" TO carma_db_sync;
GRANT ALL ON "ConsultationType_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"ConsultationType"', 'id'), max(id)) from "ConsultationType";
