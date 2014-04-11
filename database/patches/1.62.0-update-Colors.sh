#!/bin/bash -e

$PSQL -c 'drop table if exists "Colors"'
$PSQL -f baseline/3-dictionaries/26-Colors.sql
