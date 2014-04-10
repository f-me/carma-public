CREATE OR REPLACE FUNCTION fillValidUntil ()
RETURNS trigger AS $$
DECLARE
  vf integer;
BEGIN
  SELECT validFor INTO STRICT vf FROM "SubProgram" WHERE id = NEW.subprogram;
  IF vf IS NULL THEN
     RETURN NEW;
  END IF;

  NEW.validUntil := NEW.validSince + vf;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER "Contract_validUntil_update" BEFORE UPDATE OF validSince
ON "Contract"
FOR EACH ROW
WHEN (OLD.validSince IS DISTINCT FROM NEW.validSince
      AND NEW.validSince IS NOT NULL
      AND NEW.subprogram IS NOT NULL
      AND NEW.validUntil IS NULL)
EXECUTE PROCEDURE fillValidUntil();

CREATE TRIGGER "Contract_validUntil_insert" BEFORE INSERT
ON "Contract"
FOR EACH ROW
WHEN (NEW.validSince IS NOT NULL
      AND NEW.subprogram IS NOT NULL
      AND NEW.validUntil IS NULL)
EXECUTE PROCEDURE fillValidUntil();


CREATE OR REPLACE FUNCTION fillCheckPeriod ()
RETURNS trigger AS $$
DECLARE
  cp integer;
BEGIN
  SELECT checkPeriod INTO STRICT cp FROM "SubProgram" WHERE id = NEW.subprogram;
  IF cp IS NULL THEN
     RETURN NEW;
  END IF;

  NEW.checkPeriod := cp;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER "Contract_checkPeriod" BEFORE INSERT
ON "Contract"
FOR EACH ROW
WHEN (NEW.subprogram IS NOT NULL
      AND NEW.checkPeriod IS NULL)
EXECUTE PROCEDURE fillCheckPeriod();
