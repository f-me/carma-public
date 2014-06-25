ALTER TABLE "Contract" ADD COLUMN fromArc bool NOT NULL DEFAULT false;
ALTER TABLE "Contract" ADD COLUMN extra json;

ALTER TABLE "SubProgram" ADD COLUMN leader bool NOT NULL DEFAULT false;

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'leader', r, w FROM "FieldPermission"
WHERE model = 'SubProgram' AND field = 'active';

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'fromArc', 'true', 'false' FROM "FieldPermission"
WHERE model = 'Contract' AND field = 'dixi';

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'extra', 'true', 'false' FROM "FieldPermission"
WHERE model = 'Contract' AND field = 'dixi';
