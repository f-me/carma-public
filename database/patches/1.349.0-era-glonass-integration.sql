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


-- Adding "Era Glonass" "Case" source to use it when accepting EG Call Card.
INSERT INTO "CaseSource" (id, label) VALUES (4, 'ЭРА-ГЛОНАСС');


-- Creating table for "CaseEraGlonassFailure" model.
CREATE TYPE "EraGlonassIntegrationPoint"
  AS ENUM( 'RequestForService'
         , 'BindVehicles'
         , 'ChangeProcessingStatus'
         , 'ChangeRequestStatus'
         );
CREATE TABLE "CaseEraGlonassFailure"
  ( id               SERIAL PRIMARY KEY
  , ctime            TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
  , integrationPoint "EraGlonassIntegrationPoint" NOT NULL
  , requestId        TEXT NULL
  , requestBody      JSON NULL
  , responseBody     JSON NULL
  , comment          TEXT NULL
  );
GRANT ALL ON "CaseEraGlonassFailure" TO carma_db_sync;
GRANT ALL ON "CaseEraGlonassFailure_id_seq" TO carma_db_sync;


-- Creating table for "CaseEraGlonassCreateRequest" model.
CREATE TABLE "CaseEraGlonassCreateRequest"
  ( id               SERIAL PRIMARY KEY
  , ctime            TIMESTAMP WITH TIME ZONE NOT NULL
  , caseId           INTEGER NOT NULL
  , requestId        TEXT NOT NULL
  , requestBody      JSON NOT NULL
  , FOREIGN KEY (caseId) REFERENCES "casetbl" (id)
  );
GRANT ALL ON "CaseEraGlonassCreateRequest" TO carma_db_sync;
GRANT ALL ON "CaseEraGlonassCreateRequest_id_seq" TO carma_db_sync;


-- Creating table for "EraGlonassSynchronizedContract" model.
CREATE TABLE "EraGlonassSynchronizedContract"
  ( id               SERIAL PRIMARY KEY
  , ctime            TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
  , contractId       INTEGER NOT NULL
  , vin              TEXT NOT NULL
  , isHandledByCarma BOOLEAN NOT NULL
  , lastStatusChangeTime
                     TIMESTAMP WITH TIME ZONE NULL DEFAULT CURRENT_TIMESTAMP
  , FOREIGN KEY (contractId) REFERENCES "Contract" (id)
  , UNIQUE (contractId)
  );
GRANT ALL ON "EraGlonassSynchronizedContract" TO carma_db_sync;
GRANT ALL ON "EraGlonassSynchronizedContract_id_seq" TO carma_db_sync;


COMMIT;
