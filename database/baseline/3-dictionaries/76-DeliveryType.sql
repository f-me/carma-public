CREATE TABLE "DeliveryType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "DeliveryType" (id, label) FROM stdin;
1	Авиабилеты
2	Ж/д билеты
3	Билеты на паром
4	Билеты на автобус
\.

GRANT ALL ON "DeliveryType" TO carma_db_sync;
GRANT ALL ON "DeliveryType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"DeliveryType"', 'id'), max(id)) from "DeliveryType";
