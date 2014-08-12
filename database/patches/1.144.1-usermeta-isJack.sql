ALTER TABLE usermetatbl ADD COLUMN isJack bool NOT NULL DEFAULT FALSE;

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(4, 'Usermeta', 'isJack', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
(5, 'Usermeta', 'isJack', 'true', 'false');
