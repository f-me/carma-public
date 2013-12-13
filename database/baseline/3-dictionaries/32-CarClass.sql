CREATE TABLE "CarClass"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , synonyms text[]
  );

GRANT ALL ON "CarClass" TO carma_db_sync;
GRANT ALL ON "CarClass" TO carma_search;
GRANT ALL ON "CarClass_id_seq" TO carma_db_sync;
GRANT ALL ON "CarClass_id_seq" TO carma_search;

COPY "CarClass" (id, label) FROM stdin;
1	A
2	B
3	C
4	D
5	E
6	F
7	PSA B
8	PSA M1 или коммерческие
9	PSA M2
10	PSA H
\.
