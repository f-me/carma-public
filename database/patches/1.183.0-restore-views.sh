#!/bin/bash -e

$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/7-ru-services.sql
