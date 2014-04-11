#!/bin/bash -e

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql

$PSQL -c 'drop table "SynCarMake"'
$PSQL -f baseline/3-dictionaries/24-SynCarMake.sql

$PSQL -c 'drop table "SynCarModel"'
$PSQL -f baseline/3-dictionaries/25-SynCarModel.sql

$PSQL -f baseline/3-dictionaries/27-Engine.sql
$PSQL -f baseline/3-dictionaries/28-SynEngine.sql
$PSQL -f baseline/3-dictionaries/29-Transmission.sql
$PSQL -f baseline/3-dictionaries/30-SynTransmission.sql

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql
