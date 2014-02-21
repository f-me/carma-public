ALTER TABLE "SubProgram" ADD COLUMN synonyms text[];
ALTER TABLE partnertbl ADD COLUMN synonyms text[];

INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (6, 'SubProgram', 'synonyms', 't', 't');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (7, 'SubProgram', 'synonyms', 't', 'f');
INSERT INTO "FieldPermission" (role, model, field, r, w)
VALUES (3, 'partner', 'synonyms', 't', 't');
