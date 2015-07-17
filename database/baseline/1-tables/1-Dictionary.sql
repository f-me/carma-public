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
    ARRAY['id', 'model', 'program', 'label'])
, (5, 'FieldPermission', 'Разрешения для полей', ARRAY[]::int4[], ARRAY['id', 'role', 'model', 'field'])
, (7, 'Role', 'Роли', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
, (9, 'ServiceType', 'Услуги', ARRAY[]::int4[], ARRAY['id', 'label', 'icon'])
, (10, 'ServiceInfo', 'Информация об услугах', ARRAY[11,9], ARRAY['id', 'program', 'service', 'info'])
, (11, 'Program', 'Программа', ARRAY[]::int4[], ARRAY['id', 'label'])
, (12, 'SubProgram', 'Подпрограмма', ARRAY[11], ARRAY['id', 'parent', 'label'])
, (13, 'VinFormat', 'Форматы VIN', ARRAY[]::int4[], ARRAY['id', 'label'])
, (14, 'Colors', 'Цвета', ARRAY[]::int4[], ARRAY['id', 'value', 'label'])
, (15, 'ProgramType', 'Типы программ', ARRAY[]::int4[], ARRAY['id', 'label'])
, (16, 'Engine', 'Типы двигателя', ARRAY[]::int4[], ARRAY['id', 'label'])
, (17, 'Transmission', 'Коробки передач', ARRAY[]::int4[], ARRAY['id', 'label'])
, (19, 'CheckType', 'Виды ТО', ARRAY[]::int4[], ARRAY['id', 'label'])
, (20, 'CarClass', 'Классы автомобилей', ARRAY[]::int4[], ARRAY['id', 'label'])
, (21, 'BusinessRole', 'Бизнес-роли', ARRAY[]::int4[], ARRAY['id', 'label'])
, (22, 'SmsTemplate', 'Шаблон СМС', ARRAY[]::int4[], ARRAY['id', 'label'])
, (25, 'Wazzup', 'Диагностика: что случилось',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (26, 'System', 'Диагностика: системы',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (27, 'Part', 'Диагностика: узлы/детали',
       ARRAY[]::int4[], ARRAY['id', 'parent', 'label'])
, (28, 'Cause', 'Диагностика: причины неисправности',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (29, 'Suggestion', 'Диагностика: рекомендации',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (30, 'ClientRefusalReason', 'Причины отказа клиента',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (31, 'PartnerRefusalReason', 'Причины отказа партнёра',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (32, 'ContractCheckStatus', 'Статусы проверки контракта',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (33, 'TowType', 'Типы эвакуации',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (34, 'TechType', 'Типы техпомощи',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (35, 'PaymentType', 'Типы оплаты',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (36, 'CaseStatus', 'Статусы кейса',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (37, 'ServiceStatus', 'Статусы услуги',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (38, 'LegalForm', 'Формы организации',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (39, 'TaxScheme', 'Формы налогообложения',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (40, 'ActionType', 'Названия действий',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (41, 'ActionResult', 'Результаты действий',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (42, 'DeferTime', 'Интервалы откладывания действий',
       ARRAY[]::int4[], ARRAY['id', 'label', 'time'])
, (43, 'FalseCall', 'Ложный вызов',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (44, 'Satisfaction', 'Клиент доволен',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (45, 'Usermeta', 'Пользователи',
       ARRAY[]::int4[], ARRAY['id', 'login', 'realName'])
, (46, 'AvayaEventType', 'Типы событий AVAYA',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (47, 'VDN', 'Телефонные линии (VDN)',
       ARRAY[]::int4[], ARRAY['number', 'label'])
, (48, 'VipNumber', 'VIP-номера',
       ARRAY[]::int4[], ARRAY['number'])
, (49, 'ConsultationType', 'Типы консультации',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (50, 'ConsultationResult', 'Результаты консультации',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (51, 'Complication', 'Типы осложнений',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (52, 'CaseSource', 'Источники кейса',
       ARRAY[]::int4[], ARRAY['id', 'label'])
, (53, 'TowerType', 'Виды эвакуатора',
       ARRAY[]::int4[], ARRAY['id', 'label'])
;

GRANT SELECT ON "Dictionary" TO carma_db_sync;
