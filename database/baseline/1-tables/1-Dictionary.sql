CREATE TABLE "Dictionary"
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4[] default array[]::int4[]
  ,majorFields text[] default array[]::text[]
  );

INSERT INTO "Dictionary" (id, name, description, parent, majorFields) VALUES
  (0, 'CarMake', 'Марка машины', ARRAY[]::int4[], ARRAY['id', 'label'])
, (1, 'CarModel', 'Модель машины', ARRAY[0], ARRAY['id', 'parent', 'label', 'info'])
, (2, 'City', 'Город', ARRAY[]::int4[], ARRAY['id', 'label'])
, (3, 'Region', 'Регион', ARRAY[]::int4[], ARRAY['id', 'label'])
, (4, 'ConstructorFieldOption', 'Конструктор экранов',
    ARRAY[]::int4[],
    ARRAY['id', 'screen', 'model', 'program', 'label'])
, (5, 'FieldPermission', 'Разрешения для полей', ARRAY[]::int4[], ARRAY['id', 'role', 'model', 'field'])
, (6, 'SmsTemplate', 'Шаблон СМС', ARRAY[]::int4[], ARRAY['id', 'label'])
, (7, 'Role', 'Роли', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
, (9, 'ServiceNames', 'Услуги', ARRAY[]::int4[], ARRAY['id', 'value', 'label', 'icon'])
, (10, 'ServiceInfo', 'Информация об услугах', ARRAY[11,9], ARRAY['id', 'program', 'service', 'info'])
, (11, 'Program', 'Программа', ARRAY[]::int4[], ARRAY['id', 'label'])
, (12, 'SubProgram', 'Подпрограмма', ARRAY[11], ARRAY['id', 'parent', 'label'])
, (13, 'VinFormat', 'Форматы VIN', ARRAY[]::int4[], ARRAY['id', 'label'])
, (14, 'Colors', 'Цвета', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
, (15, 'ProgramType', 'Типы программ', ARRAY[]::int4[], ARRAY['id', 'label'])
, (16, 'Engine', 'Типы двигателя', ARRAY[]::int4[], ARRAY['id', 'label'])
, (17, 'Transmission', 'Коробки передач', ARRAY[]::int4[], ARRAY['id', 'label'])
, (18, 'LegalForm', 'Формы организации', ARRAY[]::int4[], ARRAY['id', 'label'])
, (19, 'CheckType', 'Вид ТО', ARRAY[]::int4[], ARRAY['id', 'label'])
, (20, 'CarClass', 'Классы автомобилей', ARRAY[]::int4[], ARRAY['id', 'label'])
, (21, 'BusinessRole', 'Бизнес-роли', ARRAY[]::int4[], ARRAY['id', 'label'])
;

GRANT SELECT ON "Dictionary" TO carma_db_sync;
GRANT SELECT ON "Dictionary" TO carma_search;
