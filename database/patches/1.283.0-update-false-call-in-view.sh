#!/bin/bash

$PSQL -c 'drop view "Услуги"'
$PSQL -f baseline/5-views/7-ru-services.sql
