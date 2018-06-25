BEGIN;

-- Adding "eraGlonassParticipant" field to "SubProgram" model
ALTER TABLE "SubProgram"
  ADD COLUMN eraGlonassParticipant
  BOOLEAN NOT NULL DEFAULT FALSE;

-- Copying field permissions of "SubProgram" model
-- from "smsProgram" to "eraGlonassParticipant" field
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  ( SELECT role, model, 'eraGlonassParticipant', r, w
      FROM "FieldPermission"
      WHERE model = 'SubProgram' AND field = 'smsProgram'
  );

-- Adding "isCreatedByEraGlonass" field to "Case" model
ALTER TABLE "casetbl"
  ADD COLUMN isCreatedByEraGlonass
  BOOLEAN NOT NULL DEFAULT FALSE;

-- Copying field permissions of "Case" model
-- from "id" to "isCreatedByEraGlonass" field
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  ( SELECT role, model, 'isCreatedByEraGlonass', r, w
      FROM "FieldPermission"
      WHERE model = 'Case' AND field = 'id'
  );

COMMIT;
