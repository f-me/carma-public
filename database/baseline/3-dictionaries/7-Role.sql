CREATE TABLE "Role"
  (id    SERIAL PRIMARY KEY
  ,value text UNIQUE NOT NULL
  ,label text UNIQUE NOT NULL
  ,isBack bool
  );

GRANT SELECT ON "Role" TO carma_search;
GRANT ALL ON "Role" TO carma_db_sync;

INSERT INTO "Role" (id, value, label,isBack) VALUES
  (1, 'core', 'Экран кейса и базовые поля','f')
, (2, 'call', 'Звонок','f')
, (3, 'parguy', 'Администрирование партнёров','f')
, (4, 'userAdmin', 'Администрирование пользователей','f')
, (5, 'userViewer', 'Просмотр справочника пользователей','f')
, (6, 'lovAdmin', 'Администрирование справочников','f')
, (7, 'lovViewer', 'Просмотр справочников','f')
, (8, 'reportManager', 'Аналитик по отчётам','f')
, (9, 'billManager', 'Управляющий счетами','f')
, (10, 'billChecker', 'Менеджер по счетам','f')
, (11, 'vinAdmin', 'Загрузка VIN','f')
, (12, 'supervisor', 'Супервизор','f')
, (13, 'head', 'Глава РКЦ','t')
, (14, 'back', 'Работа с бэкофисом','f')
, (15, 'psaanalyst', 'Аналитик PSA','f')
, (16, 'searchCase', 'Поиск услуг','f')
, (17, 'searchCall', 'Поиск звонков','f')
, (18, 'searchContract', 'Поиск контрактов','f')
, (19, 'partner', 'Пользователь экрана контрактов','f')
, (20, 'contract_admin', 'Администратор контрактов','f')
, (21, 'dealer', 'Дилер','f')

, (22, 'bo_qa', 'БО: Менеджер по качеству','t')
, (23, 'bo_order', 'БО: Заказ услуги','t')
, (24, 'bo_control', 'БО: Контроль услуги','t')
, (25, 'bo_account', 'БО: Бухгалтер','t')
, (26, 'bo_director', 'БО: Директор','t')
, (27, 'bo_analyst', 'БО: Аналитик','t')
, (28, 'bo_bill', 'БО: Операции со счетами','t')
, (29, 'bo_parguy', 'БО: Менеджер по партнёрам','t')
, (30, 'bo_close', 'БО: Закрытие кейсов','t')
, (31, 'bo_dealer', 'БО: Аналитик по работе с дилерами','t')
, (32, 'vwfake', 'Секретная роль vwfake','f')
, (33, 'front', 'Оператор Front Office','f')
;
