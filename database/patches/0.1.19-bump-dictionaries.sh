#!/bin/bash -e

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql

$PSQL -f baseline/3-dictionaries/31-CheckType.sql
$PSQL -f baseline/3-dictionaries/32-CarClass.sql
$PSQL -f baseline/3-dictionaries/33-LegalForm.sql

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
