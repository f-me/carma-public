CREATE TABLE "ActionType"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL CHECK (label <> '')
  , desc text
  , priority int4 NOT NULL
  );

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Заказ услуги', 1, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Сообщить клиенту о договорённости', 3, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнить статус оказания услуги', 4, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Уточнить после оказания услуги', 6, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Закрытие услуги', 7, 3);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Отказ от услуги', 9, 1);

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Требуется дополнительная информация', 19, 1);

SELECT setval(pg_get_serial_sequence('"ActionType"', 'id'), max(id)) from "ActionType";
