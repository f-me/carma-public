#!/usr/bin/env bash

$PSQL -f baseline/3-dictionaries/77-TowSort.sql
$PSQL -f baseline/3-dictionaries/78-BikeTowType.sql

$PSQL -c 'DROP TABLE "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
