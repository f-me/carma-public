#!/usr/bin/env bash

$PSQL -c 'drop view "Услуги"'
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -c 'drop view "Отказы партнеров"'
$PSQL -f baseline/5-views/4-ru-partner-refusals.sql
