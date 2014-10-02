#!/bin/bash -e

$PSQL -f baseline/3-dictionaries/19-ServiceType.sql

$PSQL -c 'drop table "Dictionary"'
$PSQL -f baseline/1-tables/1-Dictionary.sql

$PSQL -c "UPDATE \"FieldPermission\" SET model='ServiceType' WHERE model='ServiceNames'"
$PSQL -c "DELETE FROM \"FieldPermission\" WHERE model='ServiceType' AND field='value'"

#bump
