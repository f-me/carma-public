${PSQL} <<EOF

BEGIN;

`cat baseline/3-dictionaries/67-ProcessingConfig.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (13, 'ProcessingConfig', 'actionsFirst', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES (13, 'ProcessingConfig', 'afterCallSeconds', 't', 't');

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

END;
EOF
