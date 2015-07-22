#!/bin/bash -e

$PSQL -f baseline/3-dictionaries/73-UrgentServiceReason.sql
$PSQL -f baseline/3-dictionaries/74-Activity.sql
$PSQL -f baseline/3-dictionaries/75-RequestType.sql
$PSQL -f baseline/3-dictionaries/76-DeliveryType.sql

$PSQL -c 'DROP TABLE "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
