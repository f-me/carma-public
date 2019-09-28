BEGIN;

DROP TABLE "EraGlonassCaseStatusUpdate";

-- Creating table for "EraGlonassCaseStatusUpdate" model.
CREATE TABLE "EraGlonassCaseStatusUpdate"
  ( id               SERIAL PRIMARY KEY
  , ctime            TIMESTAMP WITH TIME ZONE NOT NULL

  , -- Instead of duplicating latest state just update this field
    mtime            TIMESTAMP WITH TIME ZONE NULL

  , caseId           INTEGER NOT NULL

  , -- One of: 'WORK_IN_PROGRESS', 'DONE' and 'CLIENT_DENIAL'
    newCaseStatus    TEXT NOT NULL

  , isProcessed      BOOLEAN NOT NULL DEFAULT FALSE
  , processTime      TIMESTAMP WITH TIME ZONE NULL

  -- Additional meta fields
  , customerName     TEXT NULL
  , customerPhone    TEXT NULL
  , terminalPhone    TEXT NULL

  , FOREIGN KEY (caseId) REFERENCES "casetbl" (id)
  );
GRANT ALL ON "EraGlonassCaseStatusUpdate" TO carma_db_sync;
GRANT ALL ON "EraGlonassCaseStatusUpdate_id_seq" TO carma_db_sync;


-- Indices for "EraGlonassCaseStatusUpdate" table.
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (ctime);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (mtime);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (caseId);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (newCaseStatus);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (isProcessed);
CREATE INDEX ON "EraGlonassCaseStatusUpdate" (processTime);


COMMIT;
