UPDATE servicetbl SET status = 2 WHERE status IS NULL;
ALTER TABLE servicetbl ALTER COLUMN status SET NOT NULL,
ALTER COLUMN status SET DEFAULT 2;

UPDATE casetbl SET caseStatus = 1 WHERE caseStatus IS NULL;
ALTER TABLE casetbl ALTER COLUMN caseStatus SET NOT NULL,
ALTER COLUMN caseStatus SET DEFAULT 1;
