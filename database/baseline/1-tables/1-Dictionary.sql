CREATE TABLE "Dictionary"
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES "Dictionary"
  ,majorFields text[] default array[]::text[]
  );

INSERT INTO "Dictionary" (id, name, description, parent, majorFields) VALUES
  (0, 'CarMake', 'Марка машины', null, ARRAY['id', 'label'])
, (1, 'CarModel', 'Модель машины', 0, ARRAY['id', 'parent', 'label'])
, (2, 'City', 'Город', 0, ARRAY['id', 'label'])
, (3, 'Region', 'Регион', 0, ARRAY['id', 'label'])
, (4, 'NewCaseField', 'Поля для экрана нового кейса', null, ARRAY['id', 'program', 'label'])
, (5, 'FieldPermission', 'Разрешения для полей', null, ARRAY['id', 'role', 'model', 'field'])
, (6, 'SmsTemplate', 'Шаблон СМС', null, ARRAY['id', 'label'])
, (7, 'Role', 'Роли', null, ARRAY['id', 'value', 'label'])
, (8, 'ProgramInfo', 'Информация о программах', null, ARRAY['id', 'program', 'info'])
, (9, 'ServiceInfo', 'Информация об услугах', null, ARRAY['id', 'program', 'service', 'info'])
, (10, 'Program', 'Программа', null, ARRAY['id', 'label'])
, (11, 'SubProgram', 'Подпрограмма', 10, ARRAY['id', 'parent', 'label'])
;

GRANT SELECT ON "Dictionary" TO carma_db_sync;
GRANT SELECT ON "Dictionary" TO carma_search;
