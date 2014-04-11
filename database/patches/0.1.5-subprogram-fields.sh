#!/bin/bash -e

$PSQL -c 'ALTER TABLE calltbl ADD subprogram int'
$PSQL -c 'ALTER TABLE casetbl ADD subprogram int'
$PSQL -c 'drop table "FieldPermission"'
$PSQL -c 'drop table "Role"'
$PSQL -f baseline/3-dictionaries/7-Role.sql
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
