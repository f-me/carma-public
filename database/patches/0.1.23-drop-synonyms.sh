#!/bin/bash -e

$PSQL -c 'drop table "SynCarMake"'
$PSQL -c 'drop table "SynCarModel"'
$PSQL -c 'drop table "SynEngine"'
$PSQL -c 'drop table "SynTransmission"'

$PSQL -c 'drop table "CarModel"'
$PSQL -f baseline/3-dictionaries/5-CarModel.sql
$PSQL -c 'drop table "CarMake"'
$PSQL -f baseline/3-dictionaries/4-CarMake.sql
$PSQL -c 'drop table "Engine"'
$PSQL -f baseline/3-dictionaries/27-Engine.sql
$PSQL -c 'drop table "Transmission"'
$PSQL -f baseline/3-dictionaries/29-Transmission.sql
$PSQL -c 'drop table "CheckType"'
$PSQL -f baseline/3-dictionaries/31-CheckType.sql
$PSQL -c 'drop table "CarClass"'
$PSQL -f baseline/3-dictionaries/32-CarClass.sql
$PSQL -c 'drop table "LegalForm"'
$PSQL -f baseline/3-dictionaries/33-LegalForm.sql

$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/1-tables/8-FieldPermission.sql

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
