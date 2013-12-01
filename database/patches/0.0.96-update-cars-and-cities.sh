#!/bin/bash -e

$PSQL -c 'drop table "SynCarModel"'
$PSQL -c 'drop table "CarModel"'
$PSQL -c 'drop table "City"'
$PSQL -f baseline/3-dictionaries/5-CarModel.sql
$PSQL -f baseline/3-dictionaries/6-City.sql
$PSQL -f baseline/3-dictionaries/25-SynCarModel.sq
