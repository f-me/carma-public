$PSQL <<EOF

BEGIN;

DROP FUNCTION get_kpi_calls(integer[],timestamptz,timestamptz);

DROP FUNCTION get_kpi_calls_days(integer[],timestamptz,timestamptz);

DROP FUNCTION group_kpi_calls(timestamptz,timestamptz);

`cat baseline/2-functions/2-kpis.sql`

END;
EOF
