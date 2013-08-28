#!/bin/bash -e

$PSQL -c "DROP VIEW servicesview"
$PSQL -f baseline/5-views/0-services-view.sql
$PSQL -c "GRANT SELECT ON servicesview TO carma_db_sync"
