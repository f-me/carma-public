#!/bin/bash -e

$PSQL -c 'drop table if exists "Sms"'
HAS_ROLE=`$PSQL -t -c "select * from pg_roles where rolname='carma_sms'"`
if [[ $HAS_ROLE == "" ]] ; then
  $PSQL -c "create role carma_sms with password 'pass' login"
fi
$PSQL -f baseline/3-dictionaries/27-Sms.sql
