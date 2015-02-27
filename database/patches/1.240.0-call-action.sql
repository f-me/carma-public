BEGIN;

INSERT INTO "ActionType" (label, id, priority)
VALUES ('Звонок', 100, 1);

INSERT INTO "ActionResult" (label, id)
VALUES ('Звонок завершён', 100);

ALTER TABLE actiontbl ADD COLUMN callId int4 REFERENCES calltbl;

INSERT INTO "FieldPermission"
(role, model, field, r, w)
SELECT role, model, 'callId', r, w
FROM "FieldPermission"
WHERE model = 'Action' AND field='caseId';

END;
