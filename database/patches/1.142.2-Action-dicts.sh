#!/bin/bash -e

$PSQL -c 'drop table "ActionResult"'
$PSQL -c 'drop table "ActionName"'

$PSQL -f baseline/3-dictionaries/0-ActionType.sql
$PSQL -f baseline/3-dictionaries/1-ActionResult.sql
$PSQL -f baseline/3-dictionaries/59-DeferTime.sql

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql

#### bump
