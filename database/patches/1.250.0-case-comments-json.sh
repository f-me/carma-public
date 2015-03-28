${PSQL} <<EOF

DROP VIEW "Услуги";

ALTER TABLE casetbl ALTER COLUMN comments TYPE json USING comments::json;

`cat baseline/5-views/7-ru-services.sql`

EOF
