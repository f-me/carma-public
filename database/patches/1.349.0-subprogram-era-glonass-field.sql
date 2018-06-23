BEGIN;

-- Adding "eraGlonassParticipant" field to "SubProgram" model
ALTER TABLE "SubProgram"
  ADD COLUMN eraGlonassParticipant
  BOOLEAN NOT NULL DEFAULT FALSE;

-- Copying field permissions from "smsProgram" to "eraGlonassParticipant"
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  ( SELECT role, model, 'eraGlonassParticipant', r, w
      FROM "FieldPermission"
      WHERE model = 'SubProgram' AND field = 'smsProgram'
  );

COMMIT;
