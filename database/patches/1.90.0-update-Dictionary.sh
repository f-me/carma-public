#!/bin/bash -e

$PSQL -c 'drop table if exists "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -c 'drop table "Sms"'
$PSQL -f baseline/3-dictionaries/27-Sms.sql

$PSQL -f baseline/3-dictionaries/43-SmsTokenName.sql
$PSQL -f baseline/3-dictionaries/44-SmsTokenValue.sql
