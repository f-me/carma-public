#!/bin/bash -e

# servicesview dropped in 1.59.0

$PSQL -f baseline/5-views/2-services-view.sql
