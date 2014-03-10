#!/bin/bash -e

$PSQL -c 'drop table "SubProgram"'
$PSQL -c 'drop table "Program"'
$PSQL -f baseline/3-dictionaries/22-Program.sql
$PSQL -f baseline/3-dictionaries/23-SubProgram.sql
