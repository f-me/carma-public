ALTER TABLE usermetatbl ADD COLUMN lastAvayaSnapshot json NOT NULL DEFAULT '{}'::json;
ALTER TABLE casetbl ADD COLUMN acStart timestamptz;

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (1, 'Usermeta', 'lastAvayaSnapshot', 't', 'f');
