#!/bin/bash -e

$PSQL -c 'ALTER TABLE calltbl ADD COLUMN enddate timestamp with time zone'
$PSQL -c 'DROP TABLE "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

# bump
