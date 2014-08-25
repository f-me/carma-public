INSERT INTO "ActionResult" (label, id)
VALUES ('Закрыто супервизором', 32);

SELECT setval(pg_get_serial_sequence('"ActionResult"', 'id'), max(id)) from "ActionResult";
