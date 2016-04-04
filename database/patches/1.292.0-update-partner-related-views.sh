#!/bin/bash
$PSQL -f baseline/5-views/5-ru-partners.sql
$PSQL -f baseline/5-views/8-ru-services-priority.sql
$PSQL -f baseline/2-functions/1-geo-within.sql
