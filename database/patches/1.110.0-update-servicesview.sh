#!/bin/bash -e


# Dropped in 1.105.0
$PSQL -f baseline/5-views/0-allservices-view.sql

# Dropped in 1.99.0
$PSQL -f baseline/5-views/2-services-view.sql
