ALTER TABLE usermetatbl ADD COLUMN isJack bool NOT NULL DEFAULT FALSE;

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('userAdmin', 'Usermeta', 'isJack', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('userViewer', 'Usermeta', 'isJack', 'true', 'false');
