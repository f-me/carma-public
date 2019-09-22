BEGIN;


-- Indices for "EraGlonassSynchronizedContract" table.
CREATE INDEX ON "EraGlonassSynchronizedContract" (ctime);
CREATE INDEX ON "EraGlonassSynchronizedContract" (contractId);
CREATE INDEX ON "EraGlonassSynchronizedContract" (vin);
CREATE INDEX ON "EraGlonassSynchronizedContract" (isHandledByCarma);
CREATE INDEX ON "EraGlonassSynchronizedContract" (lastStatusChangeTime);


-- Indices for "CaseEraGlonassFailure" table.
CREATE INDEX ON "CaseEraGlonassFailure" (ctime);
CREATE INDEX ON "CaseEraGlonassFailure" (integrationPoint);
CREATE INDEX ON "CaseEraGlonassFailure" (requestId);
CREATE INDEX ON "CaseEraGlonassFailure" (comment);
CREATE INDEX ON "CaseEraGlonassFailure" (repeats);


COMMIT;
