
$PSQL <<EOF

BEGIN;

DROP VIEW "Отказы партнеров";
`cat baseline/5-views/4-ru-partner-refusals.sql`

END;
EOF
