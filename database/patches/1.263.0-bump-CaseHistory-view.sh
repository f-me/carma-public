#!/bin/bash -e

$PSQL -c 'DROP VIEW "CaseHistory";'
$PSQL -f baseline/5-views/12-case-history.sql

# bump
