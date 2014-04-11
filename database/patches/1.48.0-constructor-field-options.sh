#!/bin/bash -e

$PSQL -f baseline/3-dictionaries/38-ConstructorFieldOption.sql
$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

# bump
