#!/bin/bash -e

$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/5-views/8-ru-services-priority.sql
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/5-views/4-ru-partner-refusals.sql
$PSQL -f baseline/5-views/2-services-view.sql
