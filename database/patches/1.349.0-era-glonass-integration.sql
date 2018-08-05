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

-- Adding new car engine types to CaRMa "Engine" model
-- which could be provided by EG side.
INSERT INTO "Engine"
  (id, label)
  VALUES -- Hydrogen
         (3, 'Водород')
       , -- Electricity
         (4, 'Электричество')
       , -- LPG - Liquefied Petroleum Gas (propane)
         -- See https://ru.wikipedia.org/wiki/Сжиженные_углеводородные_газы
         --     (about Russian abbreviation).
         (5, 'СУГ')
       , -- LNG - Liquefied Natural Gas
         -- See https://ru.wikipedia.org/wiki/Сжиженный_природный_газ
         --     (about Russian abbreviation).
         (6, 'СПГ')
       ;

COMMIT;
