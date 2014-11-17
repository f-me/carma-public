#!/bin/bash -e

$PSQL -c 'DROP VIEW IF EXISTS "Услуги"'
$PSQL -c 'DROP VIEW IF EXISTS "servicesview"'

$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/5-views/2-services-view.sql
