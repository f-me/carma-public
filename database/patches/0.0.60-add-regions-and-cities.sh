#!/bin/bash -e


$PSQL -c 'drop table regiontbl'
$PSQL -c 'drop table "City"'
$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql
$PSQL -f baseline/3-dictionaries/6-City.sql
$PSQL -f baseline/3-dictionaries/16-Region.sql

$PSQL << EOF
insert into "FieldPermission" (role, model, field, r, w) values
('all', 'Region', 'id',     true, true),
('all', 'Region', 'label',  true, true),
('all', 'Region', 'cities', true, true),
('all', 'City',   'id',     true, true),
('all', 'City',   'value',  true, true),
('all', 'City',   'label',  true, true),
('all', 'City',   'timezone',  true, true);
EOF
