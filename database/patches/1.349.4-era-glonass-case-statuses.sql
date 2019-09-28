BEGIN;


-- Creating table for "EraGlonassCaseStatusUpdate" model.
CREATE TABLE "EraGlonassCaseStatusUpdate"
  ( id               SERIAL PRIMARY KEY
  , ctime            TIMESTAMP WITH TIME ZONE NOT NULL
  , caseId           INTEGER NOT NULL
  , newCaseStatus    INTEGER NOT NULL
  , isProcessed      BOOLEAN NOT NULL DEFAULT FALSE
  , processTime      TIMESTAMP WITH TIME ZONE NULL

  -- Additional meta fields
  , customerName     Text NULL
  , customerPhone    Text NULL
  , terminalPhone    Text NULL

  , FOREIGN KEY (caseId)        REFERENCES "casetbl"    (id)
  , FOREIGN KEY (newCaseStatus) REFERENCES "CaseStatus" (id)
  );
GRANT ALL ON "EraGlonassCaseStatusUpdate" TO carma_db_sync;
GRANT ALL ON "EraGlonassCaseStatusUpdate_id_seq" TO carma_db_sync;


-- Indices for "EraGlonassCaseStatusUpdate" table.
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (ctime);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (caseId);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (newCaseStatus);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (isProcessed);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (processTime);


-- Indices for "CaseEraGlonassCreateRequest" table.
CREATE INDEX ON "CaseEraGlonassCreateRequest" (ctime);
CREATE INDEX ON "CaseEraGlonassCreateRequest" (caseId);
CREATE INDEX ON "CaseEraGlonassCreateRequest" (requestId);


COMMIT;
