$PSQL <<EOF

BEGIN;

DROP VIEW "Звонки";

`cat baseline/5-views/3-ru-calls.sql`

END;
EOF
