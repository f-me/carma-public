ALTER TABLE "Role" ADD COLUMN hidden bool NOT NULL DEFAULT 'f';

INSERT INTO "FieldPermission" (model, field, role, r, w)
VALUES ('Role', 'hidden', 6, 't', 't');
INSERT INTO "FieldPermission" (model, field, role, r, w)
VALUES ('Role', 'hidden', 7, 't', 'f');
