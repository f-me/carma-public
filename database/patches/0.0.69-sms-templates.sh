#!/bin/bash -e

$PSQL -c 'drop table smstpltbl'
$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -f baseline/3-dictionaries/18-SmsTemplate.sql
