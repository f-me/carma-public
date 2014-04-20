#!/bin/bash -e

$PSQL -f baseline/1-tables/3-Event.sql
$PSQL -f baseline/1-tables/4-UserState.sql
