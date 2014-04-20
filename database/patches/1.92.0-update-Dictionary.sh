#!/bin/bash -e

$PSQL -c 'drop table if exists "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -f baseline/3-dictionaries/45-System.sql
$PSQL -f baseline/3-dictionaries/46-Part.sql
$PSQL -f baseline/3-dictionaries/47-Cause.sql
$PSQL -f baseline/3-dictionaries/48-Suggestion.sql
$PSQL -f baseline/3-dictionaries/49-Wazzup.sql
