#!/usr/bin/env bash

$PSQL -c "alter table servicetbl add column contractor_partnerLegacy json not null default '{}'"
$PSQL -f baseline/5-views/7-ru-services.sql

