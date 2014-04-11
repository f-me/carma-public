#!/bin/bash -e

$PSQL -c 'drop table if exists "MessageToGenser"'
HAS_ROLE=`$PSQL -t -c "select * from pg_roles where rolname='carma_genser_svc'"`
if [[ $HAS_ROLE == "" ]] ; then
  $PSQL -c "create role carma_genser_svc with password 'pass' login"
fi
$PSQL -f baseline/1-tables/2-MessageToGenser.sql
