#!/bin/bash -e

$PSQL -c 'drop table "DeferTime"'
$PSQL -f baseline/3-dictionaries/59-DeferTime.sql
