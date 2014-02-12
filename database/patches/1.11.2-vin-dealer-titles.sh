#!/bin/bash -e

$PSQL -c 'drop table "VinFormat"'
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
$PSQL -f baseline/3-dictionaries/35-VinFormat.sql
