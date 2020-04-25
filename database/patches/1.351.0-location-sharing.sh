#!/usr/bin/env bash

$PSQL <<EOF
BEGIN;
create role location_sharing_svc password 'pass' login;
`cat baseline/1-tables/11-LocationSharing.sql`
END;
EOF
