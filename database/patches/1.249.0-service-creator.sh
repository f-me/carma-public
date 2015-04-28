${PSQL} <<EOF

ALTER TABLE servicetbl ADD COLUMN creator int4 REFERENCES usermetatbl;
UPDATE servicetbl SET creator = casetbl.callTaker FROM casetbl WHERE casetbl.id = servicetbl.parentId;
ALTER TABLE servicetbl ALTER COLUMN creator SET NOT NULL;

DROP VIEW "Услуги";
`cat baseline/5-views/7-ru-services.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'creator', 't', 'f'
FROM "FieldPermission"
WHERE field='createTime';

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
SELECT model, program, ord + 1, 'creator', 'Сотрудник, создавший услугу', '', 'f', 't', 'f'
FROM "ConstructorFieldOption"
WHERE model <> 1 AND field='createTime';

EOF
