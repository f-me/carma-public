ALTER TABLE casetbl ADD COLUMN caseStatus_tmp int4;

UPDATE casetbl SET caseStatus_tmp = 1 WHERE caseStatus = 's0';
UPDATE casetbl SET caseStatus_tmp = 2 WHERE caseStatus = 's0.5';
UPDATE casetbl SET caseStatus_tmp = 3 WHERE caseStatus = 's1';
UPDATE casetbl SET caseStatus_tmp = 4 WHERE caseStatus = 's2';
UPDATE casetbl SET caseStatus_tmp = 5 WHERE caseStatus = 's3';

ALTER TABLE casetbl DROP COLUMN caseStatus;
ALTER TABLE casetbl ADD COLUMN caseStatus int4 REFERENCES "CaseStatus";
UPDATE casetbl SET caseStatus = caseStatus_tmp WHERE caseStatus_tmp IS NOT NULL;
ALTER TABLE casetbl DROP COLUMN caseStatus_tmp;
