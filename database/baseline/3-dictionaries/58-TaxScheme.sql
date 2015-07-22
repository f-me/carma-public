CREATE TABLE "TaxScheme"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "TaxScheme" (id, label) FROM stdin;
1	НДС
2	УСН
3	ЕНВД
\.

GRANT ALL ON "TaxScheme" TO carma_db_sync;
GRANT ALL ON "TaxScheme_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"TaxScheme"', 'id'), max(id)) from "TaxScheme";
