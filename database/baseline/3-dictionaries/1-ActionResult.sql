CREATE TABLE "ActionResult"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL CHECK (label <> '')
  );

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга заказана, клиенту не дозвонились', 1);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга заказана', 2);

INSERT INTO "ActionResult" (label, id)
VALUES ('Требуется партнёр', 3);

INSERT INTO "ActionResult" (label, id)
VALUES ('Клиент отказался от услуги', 4);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга заказана специалистом', 5);

INSERT INTO "ActionResult" (label, id)
VALUES ('Отложить', 6);

INSERT INTO "ActionResult" (label, id)
VALUES ('Клиент согласен с условиями', 7);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга в процессе оказания/оказана', 8);

INSERT INTO "ActionResult" (label, id)
VALUES ('Партнёр найден', 9);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга упешно оказана', 10);

INSERT INTO "ActionResult" (label, id)
VALUES ('Заявка закрыта', 11);

INSERT INTO "ActionResult" (label, id)
VALUES ('Информация получена', 12);

INSERT INTO "ActionResult" (label, id)
VALUES ('Автопроизводитель одобрил', 13);

INSERT INTO "ActionResult" (label, id)
VALUES ('Автопроизводитель отказал', 14);

INSERT INTO "ActionResult" (label, id)
VALUES ('Клиент оповещён', 15);

INSERT INTO "ActionResult" (label, id)
VALUES ('Счёт введён', 16);

INSERT INTO "ActionResult" (label, id)
VALUES ('Вернуть в Back office', 17);

INSERT INTO "ActionResult" (label, id)
VALUES ('Вернуть менеджеру по счетам', 18);

INSERT INTO "ActionResult" (label, id)
VALUES ('Ок (без проверки директором и бухгалтерией)', 19);

INSERT INTO "ActionResult" (label, id)
VALUES ('Ок (без проверки директором)', 20);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга проверена РКЦ', 21);

INSERT INTO "ActionResult" (label, id)
VALUES ('Требуется доработка РКЦ', 22);

INSERT INTO "ActionResult" (label, id)
VALUES ('Услуга проверена директором', 23);

INSERT INTO "ActionResult" (label, id)
VALUES ('Требуется доработка директора', 24);

INSERT INTO "ActionResult" (label, id)
VALUES ('Проверено бухгалтерией', 25);

INSERT INTO "ActionResult" (label, id)
VALUES ('Проверено аналитиком', 26);

INSERT INTO "ActionResult" (label, id)
VALUES ('Претензия разрешена', 27);

INSERT INTO "ActionResult" (label, id)
VALUES ('Ложный вызов с выставлением счёта', 28);

INSERT INTO "ActionResult" (label, id)
VALUES ('Ложный вызов без выставления счёта', 29);

INSERT INTO "ActionResult" (label, id)
VALUES ('Коммуникация произведена', 30);

INSERT INTO "ActionResult" (label, id)
VALUES ('Коммуникация завершена, но услуга не требуется', 31);

INSERT INTO "ActionResult" (label, id)
VALUES ('Закрыто супервизором', 32);

INSERT INTO "ActionResult" (label, id)
VALUES ('Закрыто (архив)', 9000);

SELECT setval(pg_get_serial_sequence('"ActionResult"', 'id'), max(id)) from "ActionResult";

GRANT ALL ON "ActionType" TO carma_db_sync;
GRANT ALL ON "ActionType" TO carma_search;
