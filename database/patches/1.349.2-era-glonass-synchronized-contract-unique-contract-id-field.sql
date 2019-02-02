BEGIN;

-- Making `contractId` field UNIQUE.
ALTER TABLE "EraGlonassSynchronizedContract"
  ADD CONSTRAINT "EraGlonassSynchronizedContract_contractid_unique"
  UNIQUE (contractId);

COMMIT;
