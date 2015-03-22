ALTER TABLE usermetatbl ADD COLUMN grp text NOT NULL DEFAULT '';

INSERT INTO "FieldPermission"
(role, model, field, r, w)
SELECT role, model, 'grp', r, w
FROM "FieldPermission"
WHERE model = 'Usermeta' AND field='position';
