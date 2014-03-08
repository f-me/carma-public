#!/bin/bash -e

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -f baseline/3-dictionaries/36-SubProgramService.sql

$PSQL -c 'alter table "SubProgram" drop column contacts'
$PSQL -c 'alter table "SubProgram" add column contacts int4[]'
