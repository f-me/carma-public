CREATE TABLE "Dictionary"
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4[] default array[]::int4[]
  ,majorFields text[] default array[]::text[]
  );

INSERT INTO "Dictionary" (id, name, description, parent, majorFields) VALUES
  (1, 'CarMake', 'Марка машины', ARRAY[]::int4[], ARRAY['id', 'label'])
, (2, 'CarModel', 'Модель машины', ARRAY[1], ARRAY['id', 'parent', 'label', 'info'])
, (3, 'City', 'Город', ARRAY[]::int4[], ARRAY['id', 'label'])
, (4, 'Region', 'Регион', ARRAY[]::int4[], ARRAY['id', 'label'])
, (5, 'NewCaseField', 'Поля для экрана нового кейса', ARRAY[]::int4[], ARRAY['id', 'program', 'label'])
, (6, 'FieldPermission', 'Разрешения для полей', ARRAY[]::int4[], ARRAY['id', 'role', 'model', 'field'])
, (7, 'SmsTemplate', 'Шаблон СМС', ARRAY[]::int4[], ARRAY['id', 'label'])
, (8, 'Role', 'Роли', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
, (9, 'ProgramInfo', 'Информация о программах', ARRAY[12], ARRAY['id', 'program', 'info'])
, (10, 'ServiceNames', 'Услуги', ARRAY[]::int4[], ARRAY['id', 'value', 'label', 'icon'])
, (11, 'ServiceInfo', 'Информация об услугах', ARRAY[12, 10], ARRAY['id', 'program', 'service', 'info'])
, (12, 'Program', 'Программа', ARRAY[]::int4[], ARRAY['id', 'label'])
, (13, 'SubProgram', 'Подпрограмма', ARRAY[12], ARRAY['id', 'parent', 'label'])
, (14, 'SynCarMake', 'Синонимы марок', ARRAY[1], ARRAY['make', 'label'])
, (15, 'SynCarModel', 'Синонимы моделей', ARRAY[1, 2], ARRAY['make', 'model', 'label'])
, (16, 'Colors', 'Цвета', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
;

GRANT SELECT ON "Dictionary" TO carma_db_sync;
GRANT SELECT ON "Dictionary" TO carma_search;
