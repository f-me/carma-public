#!/bin/bash -e

$PSQL -f baseline/3-dictionaries/34-Contract.sql

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
