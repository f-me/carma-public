#!/bin/bash -e

$PSQL -f baseline/5-views/0-allservices-view.sql
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/5-views/8-ru-services-priority.sql
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/5-views/4-ru-partner-refusals.sql
$PSQL -f baseline/5-views/2-services-view.sql

#COMMIT=92c4e4938aeef50a04848e092f31e37376479850/1.143.0-restore-views.sh
