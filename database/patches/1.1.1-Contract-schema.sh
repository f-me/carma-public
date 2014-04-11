#!/bin/bash -e

$PSQL -c 'drop table "Contract"'
$PSQL -f baseline/3-dictionaries/34-Contract.sql
