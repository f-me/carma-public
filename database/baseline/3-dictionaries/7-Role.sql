CREATE TABLE "Role"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );

INSERT INTO "Role" (value, label) VALUES ('all', 'Все');
INSERT INTO "Role" (value, label) VALUES ('local', 'Локальный пользователь');
INSERT INTO "Role" (value, label) VALUES ('front', 'Оператор Front Office');
INSERT INTO "Role" (value, label) VALUES ('back', 'Оператор Back Office');
INSERT INTO "Role" (value, label) VALUES ('head', 'Глава РКЦ');
INSERT INTO "Role" (value, label) VALUES ('parguy', 'Менеджер по партнёрам');
INSERT INTO "Role" (value, label) VALUES ('manager', 'Менеджер по счетам');
INSERT INTO "Role" (value, label) VALUES ('accManager', 'Управляющий счетами');
INSERT INTO "Role" (value, label) VALUES ('partner', 'Партнёр');
INSERT INTO "Role" (value, label) VALUES ('supervisor', 'Супервизор');
INSERT INTO "Role" (value, label) VALUES ('director', 'Директор');
INSERT INTO "Role" (value, label) VALUES ('analyst', 'Аналитик');
INSERT INTO "Role" (value, label) VALUES ('psaanalyst', 'Аналитик PSA');
INSERT INTO "Role" (value, label) VALUES ('account', 'Бухгалтер');
INSERT INTO "Role" (value, label) VALUES ('admin', 'Администратор');
INSERT INTO "Role" (value, label) VALUES ('programman', 'Менеджер по программам');
INSERT INTO "Role" (value, label) VALUES ('op_checker', 'Аналитик по проверке кейсов');
INSERT INTO "Role" (value, label) VALUES ('op_close', 'Аналитик по закрытию кейсов');
INSERT INTO "Role" (value, label) VALUES ('op_dealer', 'Аналитик по работе с дилерами');
INSERT INTO "Role" (value, label) VALUES ('contract_admin', 'Администратор контрактов');
INSERT INTO "Role" (value, label) VALUES ('contract_user', 'Пользователь экрана контрактов');
INSERT INTO "Role" (value, label) VALUES ('vwfake', 'Секретная роль vwfake');
