CREATE TABLE "ActionType"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL CHECK (label <> '')
  , desc text
  , priority int4 NOT NULL
  );

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Заказ услуги', 1, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Заказ услуги аналитиком', 2, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Сообщить клиенту о договорённости', 3, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнить статус оказания услуги', 4, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Оповестить клиента о поиске партнёра', 5, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнения после оказания услуги', 6, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Закрыть заявку', 7, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнить информацию о ремонте у дилера/партнёра (VW, PSA)', 8, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Отказ от услуги', 9, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Согласование с производителем', 10, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Оповещение клиента об отказе производителя', 11, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Прикрепить счёт', 12, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Менеджер по счетам запросил доп. информацию', 13, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Проверка РКЦ', 14, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Проверка директором', 15, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Проверка бухгалтерией', 16, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Обработка аналитиком', 17, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Претензия', 18, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Требуется дополнительная информация', 19, 1);

SELECT setval(pg_get_serial_sequence('"ActionType"', 'id'), max(id)) from "ActionType";
