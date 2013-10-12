#!/bin/bash -e
$PSQL -c 'DROP TABLE "Role"'
$PSQL -f baseline/3-dictionaries/7-Role.sql
