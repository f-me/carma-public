#!/bin/bash

$PSQL <<EOF

BEGIN;

`cat baseline/1-tables/7-CaseComment.sql`

DROP VIEW "Услуги";

`cat baseline/5-views/7-ru-services.sql`

END;

EOF
