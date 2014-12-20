CREATE TABLE "Role"
  (id    SERIAL PRIMARY KEY
  ,value text UNIQUE NOT NULL
  ,label text UNIQUE NOT NULL
  ,isBack bool NOT NULL
  ,hidden bool NOT NULL DEFAULT 'f'
  );

GRANT SELECT ON "Role" TO carma_search;
GRANT ALL ON "Role" TO carma_db_sync;

GRANT ALL ON "Role_id_seq" TO carma_db_sync;
GRANT ALL ON "Role_id_seq" TO carma_search;

INSERT INTO "Role" (id, value, label, isBack) VALUES
  (1, 'core', 'Экран кейса и базовые поля','f')
, (2, 'call', 'Оператор Front Office','f')
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
, (34, 'dpViewer', 'Пользователь экрана ДиП','f')
, (35, 'programManager', 'Менеджер по программе','f')
, (40, 'sms', 'Отправка SMS','f')
, (41, 'bo_secondary', 'БО: Заказ вторичных услуг','t')
, (42, 'hacker', 'Разработчик','f')
, (43, 'bo_info', 'БО: Заказ услуги (ТДИ)','t')

, (50, 'cti', 'Доступ к CTI-панели','f')
;

SELECT setval(pg_get_serial_sequence('"Role"', 'id'), max(id)) from "Role";
