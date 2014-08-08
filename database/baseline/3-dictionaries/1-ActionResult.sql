CREATE TABLE "ActionResult"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL CHECK (label <> '')
  );

INSERT INTO "ActionResult" (label, id)
VALUES ("Услуга заказана, клиенту не дозвонились", 1);

INSERT INTO "ActionResult" (label, id)
VALUES ("Клиент согласен", 2);

INSERT INTO "ActionResult" (label, id)
VALUES ("Отказ от услуги", 4);

INSERT INTO "ActionResult" (label, id)
VALUES ("Отложить", 6);

INSERT INTO "ActionResult" (label, id)
VALUES ("Услуга в процессе оказания/оказана", 8);

INSERT INTO "ActionResult" (label, id)
VALUES ("Услуга упешно оказана", 10);

INSERT INTO "ActionResult" (label, id)
VALUES ("Закрыть заявку", 11);

INSERT INTO "ActionResult" (label, id)
VALUES ("Клиент предъявил претензию", 1030);

INSERT INTO "ActionResult" (label, id)
VALUES ("Закрыть услугу", 1040);

INSERT INTO "ActionResult" (label, id)
VALUES ("Клиенту не дозвонились", 1050);

INSERT INTO "ActionResult" (label, id)
VALUES ("Ложный вызов", 1060);

INSERT INTO "ActionResult" (label, id)
VALUES ("Ложный вызов с выставлением счёта", 28);

INSERT INTO "ActionResult" (label, id)
VALUES ("Ложный вызов без выставления счёта", 29);

INSERT INTO "ActionResult" (label, id)
VALUES ("Коммуникация произведена", 30);

INSERT INTO "ActionResult" (label, id)
VALUES ("Коммуникация завершена, но услуга не требуется ", 31);

SELECT setval(pg_get_serial_sequence('"ActionResult"', 'id'), max(id)) from "ActionResult";
