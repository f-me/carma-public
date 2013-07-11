CREATE TABLE "CallType"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "CallerType" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "CallType" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'CallType', id
    FROM Dictionary WHERE name = 'CallerType';

INSERT INTO "CallType" (value, label, parent)
  SELECT 'newCase',
         'Кейс/Создание нового кейса',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseD',
         'Претензии/На ДЦ',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseZ',
         'Претензии/На Заказчика',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoD',
         'Информация/О ДЦ',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPr',
         'Информация/О программе',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'mechanicConsOk',
         'Консультация механика помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'mechanicConsNotOk',
         'Консультация механика не помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'operatorConsOk',
         'Консультация оператора помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'operatorConsNotOk',
         'Консультация оператора не помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseCl',
         'Претензии/На клиента',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseCl',
         'Претензии/На клиента',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'newCase',
         'Кейс/Создание нового кейса',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseD',
         'Претензии/На ДЦ',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'info',
         'Запрос информации',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'other';

