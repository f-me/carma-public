#!/bin/bash -e

$PSQL -c 'drop table "VinFormat"'
$PSQL -f baseline/3-dictionaries/35-VinFormat.sql
