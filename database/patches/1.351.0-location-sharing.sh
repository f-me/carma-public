#!/usr/bin/env bash

$PSQL <<EOF
BEGIN;
-- bump this
create role location_sharing_svc password 'pass' login;
`cat baseline/1-tables/11-LocationSharing.sql`
`cat baseline/5-views/12-case-history.sql`
END;
EOF
