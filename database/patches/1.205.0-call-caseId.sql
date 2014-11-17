ALTER TABLE calltbl ADD COLUMN caseId int4 REFERENCES casetbl;

INSERT INTO "FieldPermission" (model, field, role, r, w)
VALUES ('Call', 'caseId', 2, true, true);
