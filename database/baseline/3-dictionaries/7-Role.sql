CREATE TABLE "Role"
  (id    SERIAL PRIMARY KEY
  ,value text UNIQUE NOT NULL
  ,label text UNIQUE NOT NULL
  );

GRANT SELECT ON "Role" TO carma_db_sync;

INSERT INTO "Role" (id, value, label) VALUES
  (1, 'all', 'Все')
, (2, 'local', 'Локальный пользователь')
, (3, 'front', 'Оператор Front Office')
, (4, 'back', 'Оператор Back Office')
, (5, 'bo_control', 'Контроль услуг')
, (6, 'head', 'Глава РКЦ')
, (7, 'parguy', 'Менеджер по партнёрам')
, (8, 'manager', 'Менеджер по счетам')
, (9, 'accManager', 'Управляющий счетами')
, (10, 'partner', 'Партнёр')
, (11, 'supervisor', 'Супервизор')
, (12, 'director', 'Директор')
, (13, 'analyst', 'Аналитик')
, (14, 'psaanalyst', 'Аналитик PSA')
, (15, 'account', 'Бухгалтер')
, (16, 'admin', 'Администратор')
, (17, 'programman', 'Менеджер по программам')
, (18, 'op_checker', 'Аналитик по проверке кейсов')
, (19, 'op_close', 'Аналитик по закрытию кейсов')
, (20, 'op_dealer', 'Аналитик по работе с дилерами')
, (21, 'contract_admin', 'Администратор контрактов')
, (22, 'contract_user', 'Пользователь экрана контрактов')
, (23, 'partners_user', 'Пользователь экрана ДиП')
, (24, 'vwfake', 'Секретная роль vwfake')
;
