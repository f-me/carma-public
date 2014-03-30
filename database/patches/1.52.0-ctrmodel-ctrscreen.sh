#!/bin/bash -e

$PSQL -c 'drop table if exists "ConstructorFieldOption"'
$PSQL -c 'drop table if exists "CtrModel"'
$PSQL -c 'drop table if exists "CtrScreen"'
$PSQL -f baseline/3-dictionaries/39-CtrModel.sql
$PSQL -f baseline/3-dictionaries/40-CtrScreen.sql
$PSQL -f baseline/3-dictionaries/38-ConstructorFieldOption.sql

# bump
