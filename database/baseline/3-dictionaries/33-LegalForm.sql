CREATE TABLE "LegalForm"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , synonyms text[]
  );

GRANT ALL ON "LegalForm" TO carma_db_sync;
GRANT ALL ON "LegalForm" TO carma_search;
GRANT ALL ON "LegalForm_id_seq" TO carma_db_sync;
GRANT ALL ON "LegalForm_id_seq" TO carma_search;

COPY "LegalForm" (id, label) FROM stdin;
1	Физическое лицо
2	Юридическое лицо
\.
