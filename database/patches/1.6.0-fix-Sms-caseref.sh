#!/bin/bash -e

$PSQL -c 'drop table if exists "Sms"'
$PSQL -f baseline/3-dictionaries/27-Sms.sql
