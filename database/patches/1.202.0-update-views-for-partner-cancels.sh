#! /bin/bash -e

$PSQL -c 'drop view if exists "Отказы партнеров"'
$PSQL -f baseline/5-views/4-ru-partner-refusals.sql
$PSQL -c 'drop table partnercanceltbl'
