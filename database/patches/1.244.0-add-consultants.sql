INSERT INTO "Role" (id, value, label, isBack)
VALUES (60, 'consultant', 'Консультант', 'f');

SELECT setval(pg_get_serial_sequence('"Role"', 'id'), max(id)) from "Role";

ALTER TABLE consultationtbl ADD COLUMN consultant int4 REFERENCES usermetatbl DEFAULT null;

INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, model, 'consultant', r, w
FROM "FieldPermission"
WHERE model = 'Consultation' AND field='consType';

INSERT INTO "ConstructorFieldOption" (model, program, ord, field, label, info, required, r, w)
SELECT model, program, ord + 1, 'consultant', 'Консультант', '', 'f', 't', 't'
FROM "ConstructorFieldOption"
WHERE model = 4 AND field='consType';
