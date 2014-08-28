CREATE SEQUENCE servicetbl_id_seq;
SELECT setval('servicetbl_id_seq', max(id)) FROM servicetbl;
ALTER TABLE servicetbl ALTER COLUMN id SET NOT NULL,
ALTER COLUMN id SET DEFAULT nextval('servicetbl_id_seq');
GRANT ALL ON "servicetbl_id_seq" TO carma_db_sync;
GRANT ALL ON "servicetbl_id_seq" TO carma_search;

CREATE SEQUENCE casetbl_id_seq;
SELECT setval('casetbl_id_seq', max(id)) FROM casetbl;
ALTER TABLE casetbl ALTER COLUMN id SET DEFAULT nextval('casetbl_id_seq');
GRANT ALL ON "casetbl_id_seq" TO carma_db_sync;
GRANT ALL ON "casetbl_id_seq" TO carma_search;

CREATE SEQUENCE actiontbl_id_seq;
SELECT setval('actiontbl_id_seq', max(id)) FROM actiontbl;
ALTER TABLE actiontbl ALTER COLUMN id SET DEFAULT nextval('actiontbl_id_seq');
GRANT ALL ON "actiontbl_id_seq" TO carma_db_sync;
GRANT ALL ON "actiontbl_id_seq" TO carma_search;
