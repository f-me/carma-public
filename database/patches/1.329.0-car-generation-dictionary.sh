#!/usr/bin/env bash

$PSQL <<EOF
BEGIN;

`cat baseline/3-dictionaries/79-CarGeneration.sql`

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'CarGeneration', field, r, w
 FROM "FieldPermission" WHERE model='CarModel' AND field IN ('label', 'parent', 'synonyms'));

END;
EOF
