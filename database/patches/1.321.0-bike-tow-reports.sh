#!/usr/bin/env bash

$PSQL <<EOF

-- bump

BEGIN;

DROP VIEW IF EXISTS "Услуги";
DROP VIEW IF EXISTS allservicesview;

`cat baseline/5-views/0-allservices-view.sql`
`cat baseline/5-views/7-ru-services.sql`
END;
EOF
