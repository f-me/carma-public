#!/bin/bash -e

$PSQL -c "create role mail_svc with password 'pass' login"
$PSQL -f baseline/1-tables/2-Email.sql
