#!/bin/bash -e

$PSQL -c 'DROP VIEW IF EXISTS "Контракты"'
$PSQL -c 'DROP VIEW IF EXISTS "Услуги с приоритетами"'

$PSQL -f baseline/5-views/3-ru-calls.sql
$PSQL -f baseline/5-views/4-ru-partner-refusals.sql
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/5-views/6-ru-contracts.sql
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/5-views/8-ru-services-priority.sql

# bump2
