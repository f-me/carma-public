#!/bin/bash -e

$PSQL -c "DROP VIEW servicesview"
$PSQL -f baseline/5-views/2-services-view.sql
