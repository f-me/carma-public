#!/usr/bin/env bash

$PSQL -c 'drop view "Услуги"'
$PSQL -c 'drop view allservicesview'
$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/7-ru-services.sql
