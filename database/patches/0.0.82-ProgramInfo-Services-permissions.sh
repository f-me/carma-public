#!/bin/bash -e

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
$PSQL -f baseline/3-dictionaries/19-ProgramInfo.sql
$PSQL -f baseline/3-dictionaries/20-ServiceInfo.sql
$PSQL -f baseline/3-dictionaries/21-Services.sql
