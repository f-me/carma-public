#!/usr/bin/env bash

$PSQL -f baseline/2-functions/5-get-sms-status-label.sql
$PSQL -f baseline/5-views/10-sms-report.sql
