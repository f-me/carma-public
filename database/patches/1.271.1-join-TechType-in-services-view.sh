$PSQL <<EOF

BEGIN;

DROP VIEW "Услуги";
`cat baseline/5-views/7-ru-services.sql`

END;
EOF
