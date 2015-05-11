$PSQL <<EOF

-- bump

BEGIN;

DROP VIEW "Услуги";
DROP VIEW allservicesview;

`cat baseline/5-views/0-allservices-view.sql`
`cat baseline/5-views/7-ru-services.sql`

DROP VIEW "Отказы партнеров";
`cat baseline/5-views/4-ru-partner-refusals.sql`

END;
EOF
