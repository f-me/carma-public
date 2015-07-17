CREATE TABLE "UrgentServiceReason"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "UrgentServiceReason" (id, label) FROM stdin;
1	Услуга не приоритетная
2	Клиент с детьми за городом в мороз
3	Клиент за городом в мороз
4	Клиент в городе на дороге мешает движению
5	Клиент в городе на дороге в мороз
6	Клиент конфликтный и оставил претензию
7	ДТП (аварийный коммиссар)
\.

GRANT ALL ON "UrgentServiceReason" TO carma_db_sync;
GRANT ALL ON "UrgentServiceReason_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"UrgentServiceReason"', 'id'), max(id)) from "UrgentServiceReason";
