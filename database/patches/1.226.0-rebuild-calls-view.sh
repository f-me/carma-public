#!/bin/bash -e


$PSQL -c 'drop view if exists "Звонки"'
$PSQL -f baseline/5-views/3-ru-calls.sql
