#!/bin/bash -e

$PSQL -c 'DROP VIEW IF EXISTS "Contract_csv"'
$PSQL -c 'DROP VIEW IF EXISTS "Контракты"'

$PSQL -c 'ALTER TABLE "Contract" ALTER COLUMN makeYear TYPE int4;'
$PSQL -c 'ALTER TABLE "VinFormat" ALTER COLUMN makeYearDefault TYPE int4;'

$PSQL -f baseline/5-views/6-ru-contracts.sql
$PSQL -f baseline/5-views/9-csv-contracts.sql
