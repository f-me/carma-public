
INSERT INTO "ActionType" (label, id, priority, description)
VALUES ('Согласовать опоздание партнёра', 23, 1,
  'Требуется согласовать время опоздания партнёра с клиентом');

INSERT INTO "ActionResult" (label, id)
VALUES ('Опоздание согласовано', 33);

INSERT INTO "ActionResult" (label, id)
VALUES ('Опоздание не согласовано', 34);
