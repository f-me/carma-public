#!/bin/bash -e

$PSQL -c 'drop table "SubProgram"'
$PSQL -c 'drop table "Program"'
$PSQL -f baseline/3-dictionaries/21-ProgramType.sql
$PSQL -f baseline/3-dictionaries/22-Program.sql
$PSQL -f baseline/3-dictionaries/23-SubProgram.sql

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -c 'drop table "Role"'
$PSQL -f baseline/3-dictionaries/7-Role.sql
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
