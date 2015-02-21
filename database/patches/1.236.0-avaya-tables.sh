$PSQL <<EOF

BEGIN;

`cat baseline/3-dictionaries/62-AvayaEventType.sql`
`cat baseline/3-dictionaries/63-AvayaEvent.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'AvayaEventType', field, r, w FROM "FieldPermission" WHERE model='Satisfaction');

END;
EOF
