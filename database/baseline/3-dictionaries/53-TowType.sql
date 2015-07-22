CREATE TABLE "TowType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "TowType" (id, label) FROM stdin;
1	К дилеру
2	К дому в ночное время
3	До шиномонтажа/СТО
4	В любое другое место
\.

GRANT ALL ON "TowType" TO carma_db_sync;
GRANT ALL ON "TowType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"TowType"', 'id'), max(id)) from "TowType";
