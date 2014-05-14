CREATE TABLE "PaymentType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "PaymentType" (id, label) FROM stdin;
1	РАМК
2	Клиент
3	Смешанный
4	Клиент с возмещением
\.

GRANT ALL ON "PaymentType" TO carma_db_sync;
GRANT ALL ON "PaymentType" TO carma_search;
GRANT ALL ON "PaymentType_id_seq" TO carma_db_sync;
GRANT ALL ON "PaymentType_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"PaymentType"', 'id'), max(id)) from "PaymentType";
