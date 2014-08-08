ALTER TABLE "Usermeta" ADD COLUMN isJack bool NOT NULL DEFAULT FALSE;

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('userAdmin', 'usermeta', 'isJack', 'true', 'true');

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES
('userViewer', 'usermeta', 'isJack', 'true', 'false');
