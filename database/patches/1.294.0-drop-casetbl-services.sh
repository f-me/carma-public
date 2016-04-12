#!/usr/bin/env bash

$PSQL -c 'drop view "Услуги"'
$PSQL -c 'alter table casetbl drop column services'
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/4-indexes-for-services.sql
