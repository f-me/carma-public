#!/bin/bash -e

$PSQL -c 'drop table if exists "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -c 'drop table "FieldPermission"'
$PSQL -f baseline/3-dictionaries/8-FieldPermission.sql

$PSQL -f baseline/3-dictionaries/50-ClientRefusalReason.sql
$PSQL -f baseline/3-dictionaries/51-PartnerRefusalReason.sql
$PSQL -f baseline/3-dictionaries/52-ContractCheckStatus.sql
$PSQL -f baseline/3-dictionaries/53-TowType.sql
$PSQL -f baseline/3-dictionaries/54-TechType.sql
$PSQL -f baseline/3-dictionaries/55-PaymentType.sql
$PSQL -f baseline/3-dictionaries/56-CaseStatus.sql
$PSQL -f baseline/3-dictionaries/57-ServiceStatus.sql
