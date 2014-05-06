--------------
-- call fields
--------------

ALTER TABLE calltbl ADD COLUMN customerComment text;

ALTER TABLE calltbl ADD COLUMN wazzup_tmp int4;

UPDATE calltbl
SET wazzup_tmp = "Wazzup".id
FROM "Diagnosis0", "Wazzup"
WHERE "Diagnosis0".value = wazzup
AND "Diagnosis0".label = "Wazzup".label;

UPDATE calltbl
SET wazzup_tmp = "Wazzup".id
FROM "Wazzup"
WHERE lower(wazzup) = lower("Wazzup".label);

-- Preserve out-of-dictionary values in customerComment
UPDATE calltbl
SET customerComment = wazzup
WHERE wazzup_tmp IS NULL;

ALTER TABLE calltbl DROP COLUMN wazzup;
ALTER TABLE calltbl ADD COLUMN wazzup int4 REFERENCES "Wazzup";
UPDATE calltbl SET wazzup = wazzup_tmp WHERE wazzup_tmp IS NOT NULL;
ALTER TABLE calltbl DROP COLUMN wazzup_tmp;

--------------
-- case fields
--------------

ALTER TABLE casetbl ADD COLUMN customerComment text;

ALTER TABLE casetbl ADD COLUMN comment_tmp int4;
ALTER TABLE casetbl ADD COLUMN diagnosis1_tmp int4;
ALTER TABLE casetbl ADD COLUMN diagnosis2_tmp int4;
ALTER TABLE casetbl ADD COLUMN diagnosis3_tmp int4;
ALTER TABLE casetbl ADD COLUMN diagnosis4_tmp int4;

UPDATE casetbl
SET diagnosis1_tmp = "System".id
FROM "Diagnosis1", "System"
WHERE "Diagnosis1".value = diagnosis1
AND "Diagnosis1".label = "System".label;

UPDATE casetbl
SET diagnosis1_tmp = "System".id
FROM "System"
WHERE lower(diagnosis1) = lower("System".label);

UPDATE casetbl
SET diagnosis2_tmp = "Part".id
FROM "Diagnosis2", "Part"
WHERE "Diagnosis2".value = diagnosis2
AND "Diagnosis2".label = "Part".label;

UPDATE casetbl
SET diagnosis2_tmp = "Part".id
FROM "Part"
WHERE lower(diagnosis2) = lower("Part".label);

UPDATE casetbl
SET diagnosis3_tmp = "Cause".id
FROM "Diagnosis3", "Cause"
WHERE "Diagnosis3".value = diagnosis3
AND "Diagnosis3".label = "Cause".label;

UPDATE casetbl
SET diagnosis3_tmp = "Cause".id
FROM "Cause"
WHERE lower(diagnosis3) = lower("Cause".label);

UPDATE casetbl
SET diagnosis4_tmp = "Suggestion".id
FROM "Diagnosis4", "Suggestion"
WHERE "Diagnosis4".value = diagnosis4
AND "Diagnosis4".label = "Suggestion".label;

UPDATE casetbl
SET diagnosis4_tmp = "Suggestion".id
FROM "Suggestion"
WHERE lower(diagnosis4) = lower("Suggestion".label);

UPDATE casetbl
SET comment_tmp = "Wazzup".id
FROM "Diagnosis0", "Wazzup"
WHERE "Diagnosis0".value = comment
AND "Diagnosis0".label = "Wazzup".label;

UPDATE casetbl
SET comment_tmp = "Wazzup".id
FROM "Wazzup"
WHERE lower(comment) = lower("Wazzup".label);

-- Preserve out-of-dictionary values in customerComment
UPDATE casetbl
SET customerComment = comment
WHERE comment_tmp IS NULL;

DROP VIEW servicesview;

ALTER TABLE casetbl DROP COLUMN comment;
ALTER TABLE casetbl DROP COLUMN diagnosis1;
ALTER TABLE casetbl DROP COLUMN diagnosis2;
ALTER TABLE casetbl DROP COLUMN diagnosis3;
ALTER TABLE casetbl DROP COLUMN diagnosis4;

ALTER TABLE casetbl ADD COLUMN comment int4 REFERENCES "Wazzup";
ALTER TABLE casetbl ADD COLUMN diagnosis1 int4 REFERENCES "System";
ALTER TABLE casetbl ADD COLUMN diagnosis2 int4 REFERENCES "Part";
ALTER TABLE casetbl ADD COLUMN diagnosis3 int4 REFERENCES "Cause";
ALTER TABLE casetbl ADD COLUMN diagnosis4 int4 REFERENCES "Suggestion";

UPDATE casetbl SET comment = comment_tmp WHERE comment_tmp IS NOT NULL;
UPDATE casetbl SET diagnosis1 = diagnosis1_tmp WHERE diagnosis1_tmp IS NOT NULL;
UPDATE casetbl SET diagnosis2 = diagnosis2_tmp WHERE diagnosis2_tmp IS NOT NULL;
UPDATE casetbl SET diagnosis3 = diagnosis3_tmp WHERE diagnosis3_tmp IS NOT NULL;
UPDATE casetbl SET diagnosis4 = diagnosis4_tmp WHERE diagnosis4_tmp IS NOT NULL;

ALTER TABLE casetbl DROP COLUMN comment_tmp;
ALTER TABLE casetbl DROP COLUMN diagnosis1_tmp;
ALTER TABLE casetbl DROP COLUMN diagnosis2_tmp;
ALTER TABLE casetbl DROP COLUMN diagnosis3_tmp;
ALTER TABLE casetbl DROP COLUMN diagnosis4_tmp;
