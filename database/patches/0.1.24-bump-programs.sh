#!/bin/bash -e

$PSQL -c "DROP VIEW servicesview"
$PSQL -c "DROP VIEW allservicesview"
$PSQL -c 'drop table "Contract"'
$PSQL -c 'drop table "SubProgram"'
$PSQL -c 'drop table "ProgramInfo"'
$PSQL -c 'drop table "ServiceInfo"'
$PSQL -c 'drop table "Program"'
$PSQL -f baseline/3-dictionaries/22-Program.sql
$PSQL -f baseline/3-dictionaries/20-ServiceInfo.sql
$PSQL -f baseline/3-dictionaries/19-ProgramInfo.sql
$PSQL -f baseline/3-dictionaries/23-SubProgram.sql
$PSQL -f baseline/3-dictionaries/34-Contract.sql
$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/2-services-view.sql
