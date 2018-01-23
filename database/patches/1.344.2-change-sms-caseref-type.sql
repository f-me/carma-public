BEGIN;

DROP VIEW IF EXISTS "SMS";

-- There's some invalid garbage values like phone number '88003010880' that is
-- out of `INTEGER` range, that's why we limiting it with 9 symbols.
UPDATE "Sms" SET caseRef = ''
  WHERE caseRef !~ '^[0-9]{1,9}$'
    OR  caseRef IN ('123', '228', '654', '11111');

WITH
  int_caserefs
    AS (SELECT id, (NULLIF(caseRef, '')::INTEGER) AS caseRef FROM "Sms"),
  case_max
    AS (SELECT MIN(id) AS minid, MAX(id) AS maxid FROM casetbl),
  invalid_caserefs
    AS ( SELECT s.id, s.caseRef
           FROM int_caserefs AS s
           INNER JOIN case_max AS c ON TRUE
           WHERE s.caseRef < c.minid OR s.caseRef > c.maxid
       )

  UPDATE "Sms"
    SET caseRef = ''
    FROM invalid_caserefs
    WHERE invalid_caserefs.id = "Sms".id;

  /*
  -- This was too slow.
  SELECT t.id, t.caseRef
    FROM valid_sms AS t
    WHERE t.caseRef NOT IN (
            SELECT c.id
              FROM casetbl AS c
              INNER JOIN valid_sms AS s ON c.id = s.caseRef
          )
  */

ALTER TABLE "Sms"
  ALTER COLUMN caseRef TYPE INTEGER
  USING (NULLIF(caseRef, '')::INTEGER);

ALTER TABLE "Sms"
  ADD CONSTRAINT "Sms_caseRef_fkey" FOREIGN KEY (caseRef) REFERENCES "casetbl";

COMMIT;
