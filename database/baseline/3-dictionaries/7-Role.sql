CREATE TABLE "Role"
  (id    SERIAL PRIMARY KEY
  ,value text UNIQUE NOT NULL
  ,label text UNIQUE NOT NULL
  );

INSERT INTO "Role" (value, label) VALUES
  ('all', 'Все')
, ('local', 'Локальный пользователь')
, ('front', 'Оператор Front Office')
, ('back', 'Оператор Back Office')
, ('bo_control', 'Контроль услуг')
, ('head', 'Глава РКЦ')
, ('parguy', 'Менеджер по партнёрам')
, ('manager', 'Менеджер по счетам')
, ('accManager', 'Управляющий счетами')
, ('partner', 'Партнёр')
, ('supervisor', 'Супервизор')
, ('director', 'Директор')
, ('analyst', 'Аналитик')
, ('psaanalyst', 'Аналитик PSA')
, ('account', 'Бухгалтер')
, ('admin', 'Администратор')
, ('programman', 'Менеджер по программам')
, ('op_checker', 'Аналитик по проверке кейсов')
, ('op_close', 'Аналитик по закрытию кейсов')
, ('op_dealer', 'Аналитик по работе с дилерами')
, ('contract_admin', 'Администратор контрактов')
, ('contract_user', 'Пользователь экрана контрактов')
, ('partners_user', 'Пользователь экрана ДиП')
, ('vwfake', 'Секретная роль vwfake')
;
