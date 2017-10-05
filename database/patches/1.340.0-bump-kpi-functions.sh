#!/usr/bin/env bash

$PSQL -f baseline/2-functions/2-kpis.sql
$PSQL -f baseline/5-views/13-report_actions.sql
