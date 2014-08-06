#!/bin/bash -e

$PSQL -f baseline/5-views/2-services-view.sql
$PSQL -f baseline/5-views/3-ru-calls.sql
$PSQL -f baseline/5-views/7-ru-services.sql
