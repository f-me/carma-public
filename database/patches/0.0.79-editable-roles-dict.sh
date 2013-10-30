#!/bin/bash -e

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
