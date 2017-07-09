#!/usr/bin/env bash

$PSQL <<EOF
BEGIN;

`cat baseline/3-dictionaries/79-CarGeneration.sql`

DROP TABLE "Dictionary";
`cat baseline/1-tables/1-Dictionary.sql`

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, 'CarGeneration', field, r, w
 FROM "FieldPermission" WHERE model='CarModel' AND field IN ('label', 'parent', 'synonyms'));

ALTER TABLE "Contract" ADD COLUMN generation int4 REFERENCES "CarGeneration"(id) NULL;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, 'generation', r, w
 FROM "FieldPermission" WHERE model='Contract' AND field = 'model');

INSERT INTO "SubProgramContractPermission" (parent, contractfield, showtable, showform)
(SELECT parent, 'generation', showtable, showform
 FROM "SubProgramContractPermission" WHERE contractfield = 'model');

UPDATE "SubProgram" SET contractPermissions = rev.perms FROM
(SELECT parent, array_agg(id) AS perms
 FROM "SubProgramContractPermission" GROUP BY parent) rev WHERE rev.parent = "SubProgram".id;

ALTER TABLE casetbl ADD COLUMN car_generation int4 REFERENCES "CarGeneration"(id) NULL;

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, 'car_generation', r, w
 FROM "FieldPermission" WHERE model='Case' AND field = 'car_model');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, r, w)
(SELECT 1, program, ord + 1, 'car_generation', 'Поколение', r, w
 FROM "ConstructorFieldOption" s where model=1 AND field='car_model');

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, replace(field, 'model', 'generation'), r, w
 FROM "FieldPermission" WHERE model='VinFormat' AND field ilike 'model%');

ALTER TABLE "VinFormat" ADD COLUMN generationLoad bool NOT NULL DEFAULT FALSE;
ALTER TABLE "VinFormat" ADD COLUMN generationTitle text NOT NULL DEFAULT 'Поколение';
ALTER TABLE "VinFormat" ADD COLUMN generationDefault int4 REFERENCES "CarGeneration";
ALTER TABLE "VinFormat" ADD COLUMN generationRequired bool NOT NULL DEFAULT FALSE;

END;
EOF
