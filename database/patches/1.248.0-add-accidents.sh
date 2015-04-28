${PSQL} <<EOF

UPDATE "ActionType" SET priority = priority + 1 WHERE priority < 5;

INSERT INTO "ActionType" (label, id, priority)
VALUES ('ДТП', 22, 1);

INSERT INTO "CaseStatus" (label, id)
VALUES ('ДТП из мобильного приложения', 7);

`cat baseline/3-dictionaries/69-CaseSource.sql`

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

ALTER TABLE casetbl ADD COLUMN source int4 REFERENCES "CaseSource" NOT NULL DEFAULT 1;

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, 'CaseSource', field, r, w
FROM "FieldPermission"
WHERE model = 'PaymentType';

DROP VIEW "Услуги";

`cat baseline/5-views/7-ru-services.sql`

EOF
