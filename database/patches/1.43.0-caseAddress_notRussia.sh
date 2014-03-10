#!/bin/bash -e

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -c 'ALTER TABLE casetbl ADD COLUMN caseAddress_notRussia bool'
