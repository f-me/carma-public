#!/bin/bash -e

$PSQL -c 'drop table "Role"'
$PSQL -f baseline/3-dictionaries/7-Role.sql
