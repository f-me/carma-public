BEGIN;

ALTER TABLE servicetbl ADD COLUMN rushJob BOOL DEFAULT FALSE;

-- id#1 ("Role") - core/Экран кейса и базовые поля
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  VALUES
  (1, 'Service', 'rushJob', TRUE, FALSE);

-- TODO shift order
INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  ( SELECT ctr.id
         , program.id
         , 16
         , 'rushJob'
         , 'Аврал'
         , TRUE
         , FALSE
      FROM "Program" AS program
      INNER JOIN "CtrModel" AS ctr
        ON ctr.value IN ('Towage')
  );

COMMIT;
