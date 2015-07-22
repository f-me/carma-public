CREATE TABLE "Engine"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , synonyms text[]
  );

GRANT ALL ON "Engine" TO carma_db_sync;
GRANT ALL ON "Engine_id_seq" TO carma_db_sync;

COPY "Engine" (id, label, synonyms) FROM stdin;
1	Бензин	{TSI,FSI,TFSI,HPI,CGI,JTS,IDE,GDI}
2	Дизель	{TDI,SDI,HDI,CRDI,TDCI,DCI,CDI,CDTI,JTD}
\.

SELECT setval(pg_get_serial_sequence('"Engine"', 'id'), max(id)) from "Engine";
