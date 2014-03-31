#!/bin/bash -e

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
$PSQL -c 'drop table "ConstructorFieldOption"'
$PSQL -f baseline/3-dictionaries/38-ConstructorFieldOption.sql
