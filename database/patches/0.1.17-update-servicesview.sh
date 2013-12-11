#!/bin/bash -e

$PSQL -c "DROP VIEW servicesview"
$PSQL -c "DROP VIEW allservicesview"
$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/2-services-view.sql
