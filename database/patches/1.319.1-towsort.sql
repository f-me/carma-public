BEGIN;

ALTER TABLE towagetbl ADD COLUMN towSort int4 REFERENCES "TowSort";

INSERT INTO "FieldPermission" (role, model, field, r, w)
(SELECT role, model, 'towSort', r, w FROM "FieldPermission" WHERE model='Towage' and field='towType');

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
(SELECT model, program, ord + 1, 'towSort', 'Тип эвакуации', info, required, r, w
 FROM "ConstructorFieldOption" WHERE model=17 AND field='towType');

COMMIT;
