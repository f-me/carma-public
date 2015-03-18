${PSQL} <<EOF

`cat baseline/3-dictionaries/68-Complication.sql`

ALTER TABLE servicetbl ADD COLUMN complication int4 REFERENCES "Complication" DEFAULT null;

DROP VIEW "Услуги";
`cat baseline/5-views/7-ru-services.sql`

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'complication', r, w
FROM "FieldPermission"
WHERE model <> 'Sms' AND field='status';

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, 'Complication', field, r, w
FROM "FieldPermission"
WHERE model = 'PaymentType';

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
SELECT model, program, ord + 1, 'complication', 'Сложный случай', '', 'f', 't', 't'
FROM "ConstructorFieldOption"
WHERE model <> 1 AND field='status';

EOF
