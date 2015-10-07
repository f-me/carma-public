#!/bin/bash -e

$PSQL -c 'DROP FUNCTION group_kpi_towstartavg(timestamp with time zone, timestamp with time zone)'
$PSQL -f 'baseline/2-functions/2-kpis.sql'
