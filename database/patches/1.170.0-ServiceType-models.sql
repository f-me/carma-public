-- delete unused service types

DELETE FROM "servicetbl" WHERE type = 19;
DELETE FROM "SubProgramService" WHERE type = 19;
UPDATE "SubProgram" s SET services=remap.services
FROM
(SELECT s.id, array_agg(v.id) as services
FROM "SubProgram" s, "SubProgramService" v
WHERE v.parent=s.id
GROUP BY s.id
ORDER BY id) remap
WHERE s.id=remap.id;
DELETE FROM "ServiceType" WHERE id = 19;

DELETE FROM "FieldPermission"
WHERE model='Insurance';

ALTER TABLE "ServiceType"
ADD COLUMN model int4;

INSERT INTO "FieldPermission" (role, model, field, r, w)
select role, model, 'model', r, w from "FieldPermission"
where field='fdds' AND model='ServiceType';

UPDATE "CtrModel" SET label='ТО' WHERE id = 15;

UPDATE "ServiceType" SET model = c.id
FROM "CtrModel" c WHERE "ServiceType".label = c.label;

ALTER TABLE "ServiceType"
ALTER COLUMN model SET NOT NULL;

ALTER TABLE "ServiceType"
ADD CONSTRAINT "ServiceType_model_fkey" FOREIGN KEY (model)
REFERENCES "CtrModel"(id);

DROP VIEW "Услуги";
DROP VIEW "allservicesview";

DROP TABLE insurancetbl;
