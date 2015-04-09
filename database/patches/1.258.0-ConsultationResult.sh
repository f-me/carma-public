$PSQL <<EOF

BEGIN;

`cat baseline/3-dictionaries/72-ConsultationResult.sql`

ALTER TABLE consultationtbl ADD COLUMN consResult int4 REFERENCES "ConsultationResult";

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, 'ConsultationResult', field, r, w
FROM "FieldPermission"
WHERE model = 'PaymentType';

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'consResult', r, w
FROM "FieldPermission"
WHERE field = 'consType';

`cat baseline/5-views/0-allservices-view.sql`
`cat baseline/5-views/7-ru-services.sql`

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
SELECT model, program, ord + 1, 'consResult', 'Результат консультации', '', 'f', 't', 't'
FROM "ConstructorFieldOption"
WHERE model = 4 AND field='consType';

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

END;
EOF
