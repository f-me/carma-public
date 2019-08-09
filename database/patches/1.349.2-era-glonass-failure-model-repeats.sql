BEGIN;


-- Adding a field to store timestamps of repeated failures.
-- When a failure with the same textual comment happens again previous failure
-- record will be used instead of creating new one, and the timestamp of the
-- moment of a repeated failure will be added to this field
-- (to the list of this field).
ALTER TABLE "CaseEraGlonassFailure"
  ADD COLUMN repeats
  TIMESTAMP WITH TIME ZONE [] NULL;


COMMIT;
