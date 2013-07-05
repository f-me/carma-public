CREATE TABLE "CallerType"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('CallerType');

INSERT INTO "CallerType" (value, label) VALUES
 ('client'
 ,'Клиент');
INSERT INTO "CallerType" (value, label) VALUES
 ('contr'
 ,'Подрядчик');
INSERT INTO "CallerType" (value, label) VALUES
 ('dealer'
 ,'Дилерский центр');
INSERT INTO "CallerType" (value, label) VALUES
 ('partner'
 ,'Заказчик программы');
INSERT INTO "CallerType" (value, label) VALUES
 ('staff'
 ,'Сотрудник');
INSERT INTO "CallerType" (value, label) VALUES
 ('other'
 ,'Другое');

