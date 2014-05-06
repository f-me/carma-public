#!/bin/bash -e

$PSQL -c 'drop table if exists "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -f baseline/3-dictionaries/50-ClientRefusalReason.sql
$PSQL -f baseline/3-dictionaries/51-PartnerRefusalReason.sql
