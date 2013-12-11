#!/bin/bash -e

$PSQL -c 'drop table if exists "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table if exists "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
$PSQL -c 'drop table if exists "Colors"'
$PSQL -f baseline/3-dictionaries/26-Colors.sql
