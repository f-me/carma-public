#!/bin/bash -e

$PSQL -c 'drop table if exists "CarMaker" cascade'
$PSQL -c 'drop table if exists "CarModel" cascade'
$PSQL -f baseline/3-dictionaries/4-CarMake.sql
$PSQL -f baseline/3-dictionaries/5-CarModel.sql

$PSQL -c 'drop table if exists dictionary'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -f baseline/3-dictionaries/15-NewCaseField.sql


$PSQL << EOF
insert into "FieldPermission" (role, model, field, r, w) values
 ('all', 'CarMake', 'value', true, true)
,('all', 'CarMake', 'label', true, true)
,('all', 'CarModel', 'value', true, true)
,('all', 'CarModel', 'label', true, true)
,('all', 'CarModel', 'parent', true, true)
,('all', 'Dictionary', 'name', true, true)
,('all', 'Dictionary', 'description', true, true)
,('all', 'Dictionary', 'parent', true, true)
,('all', 'Dictionary', 'majorFields', true, true)
,('all', 'NewCaseField', 'field', true, true)
,('all', 'NewCaseField', 'program', true, true)
,('all', 'NewCaseField', 'label', true, true)
,('all', 'NewCaseField', 'r', true, true)
,('all', 'NewCaseField', 'w', true, true)
;

GRANT ALL ON "CarMake" TO carma_db_sync;
GRANT ALL ON "CarModel" TO carma_db_sync;
GRANT ALL ON "Dictionary" TO carma_db_sync;
GRANT ALL ON "NewCaseField" TO carma_db_sync;
EOF
