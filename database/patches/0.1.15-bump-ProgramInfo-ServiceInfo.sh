#!/bin/bash -e

$PSQL -c 'drop table "ProgramInfo"'
$PSQL -f baseline/3-dictionaries/19-ProgramInfo.sql

$PSQL -c 'drop table "ServiceInfo"'
$PSQL -f baseline/3-dictionaries/20-ServiceInfo.sql
