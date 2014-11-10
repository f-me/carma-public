#!/bin/bash -e

$PSQL <<EOF
BEGIN;

DROP FUNCTION get_kpi_timeinstate(integer[],tstzrange);
DROP FUNCTION get_KPI_calls(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_actions(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_controll_actions(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_sumcalls(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_sum_orderactions(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_utilization(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_avg_actdo(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_actions_relation(integer[], timestamptz, timestamptz);
DROP FUNCTION get_KPI_time_relation(integer[], timestamptz, timestamptz);

`cat baseline/2-functions/2-kpis.sql`

END;

EOF
