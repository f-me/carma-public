BEGIN;

ALTER TABLE usermetatbl ADD COLUMN stuff json NOT NULL DEFAULT '{}';

INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Usermeta', 'stuff', '1', '1');
END;
