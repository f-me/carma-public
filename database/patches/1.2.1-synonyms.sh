#!/bin/bash -e

$PSQL -c 'drop table "Contract"'
$PSQL -c 'drop table "VinFormat"'
$PSQL -c 'drop table "Transmission"'
$PSQL -c 'drop table "Engine"'
$PSQL -c 'drop table "CarModel"'
$PSQL -c 'drop table "CarMake"'
$PSQL -f baseline/3-dictionaries/4-CarMake.sql
$PSQL -f baseline/3-dictionaries/5-CarModel.sql
$PSQL -f baseline/3-dictionaries/27-Engine.sql
$PSQL -f baseline/3-dictionaries/29-Transmission.sql
$PSQL -f baseline/3-dictionaries/35-VinFormat.sql
$PSQL -f baseline/3-dictionaries/34-Contract.sql

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
