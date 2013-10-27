#!/bin/bash -e
$PSQL -c 'DROP TABLE "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
