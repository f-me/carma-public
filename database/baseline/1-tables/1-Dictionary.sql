CREATE TABLE "Dictionary"
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES "Dictionary"
  ,majorFields text[]
  );

INSERT INTO "Dictionary" (id, name, description, parent, majorFields) VALUES
  (0, 'CarMake', 'Марка машины', null, ARRAY['id', 'label'])
, (1, 'CarModel', 'Модель машины', 0, ARRAY['id', 'parent', 'label'])
, (2, 'NewCaseField', 'Поля для экрана нового кейса', null, ARRAY['id', 'program', 'label'])
, (3, 'FieldPermission', 'Разрешения для полей', null, ARRAY['id', 'role', 'model', 'field'])
;

GRANT SELECT ON "Dictionary" TO carma_db_sync;
GRANT SELECT ON "Dictionary" TO carma_search;
