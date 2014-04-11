ALTER TABLE "SubProgram" DROP COLUMN contract;
ALTER TABLE "SubProgram" ADD COLUMN template text;

DELETE FROM "FieldPermission" WHERE model='SubProgram' AND field='contract';

INSERT INTO "FieldPermission" (model, field, role, r, w)
VALUES ('SubProgram', 'template', 6, 't', 't');

INSERT INTO "FieldPermission" (model, field, role, r, w)
VALUES ('SubProgram', 'template', 7, 't', 'f');
