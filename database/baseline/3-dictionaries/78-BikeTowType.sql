CREATE TABLE "BikeTowType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "BikeTowType" (id, label) FROM stdin;
1	Мотоэвакуация
2	Мотоэвакуация 2
3	Мотоэвакуация 3
4	Мотоэвакуация 4
5	Мотоэвакуация 5
\.

GRANT ALL ON "BikeTowType" TO carma_db_sync;
GRANT ALL ON "BikeTowType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"BikeTowType"', 'id'), max(id)) from "BikeTowType";
