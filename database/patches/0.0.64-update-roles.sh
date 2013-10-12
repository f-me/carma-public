#!/bin/bash -e
$PSQL -c 'DROP TABLE "Role"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
