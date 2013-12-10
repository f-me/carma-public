CREATE TABLE "SynEngine"
  ( id     SERIAL PRIMARY KEY
  , engine int4 REFERENCES "Engine"
  , label  text UNIQUE NOT NULL
  );

GRANT ALL ON "SynEngine" TO carma_db_sync;
GRANT ALL ON "SynEngine" TO carma_search;
GRANT ALL ON "SynEngine_id_seq" TO carma_db_sync;
GRANT ALL ON "SynEngine_id_seq" TO carma_search;

COPY "SynEngine" (engine, label) FROM stdin;
1	TSI
1	FSI
1	TFSI
1	HPI
1	CGI
1	JTS
1	IDE
1	GDI
2	TDI 
2	SDI
2	HDI 
2	CRDI
2	TDCI
2	DCI
2	CDI
2	CDTI
2	JTD
\.
