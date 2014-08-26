INSERT INTO "ActionResult" (label, id)
VALUES ('Закрыто супервизором', 32);

SELECT setval(pg_get_serial_sequence('"ActionResult"', 'id'), max(id)) from "ActionResult";

UPDATE actiontbl SET result = 32 WHERE closed AND result IS NULL;
ALTER TABLE actiontbl DROP COLUMN closed;
