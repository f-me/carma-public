$PSQL <<EOF

BEGIN;

`cat baseline/3-dictionaries/64-VDN.sql`
`cat baseline/3-dictionaries/65-VipNumber.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (6, 'VDN', 'label', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (6, 'VDN', 'number', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (6, 'VDN', 'greeting', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (6, 'VDN', 'program', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (7, 'VDN', 'label', 't', 'f');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (7, 'VDN', 'number', 't', 'f');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (7, 'VDN', 'greeting', 't', 'f');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (7, 'VDN', 'program', 't', 'f');

INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (6, 'VipNumber', 'number', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (7, 'VipNumber', 'number', 't', 'f');

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

END;
EOF
