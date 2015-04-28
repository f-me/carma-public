CREATE TEMPORARY TABLE case_comments AS
SELECT id as case_id, cmts->>'date' AS cmt_ts, cmts->>'user' AS cmt_login, cmts->>'comment' AS cmt FROM
(SELECT id, json_array_elements(comments) AS cmts FROM casetbl) s;

UPDATE case_comments SET cmt_login='admin' WHERE NOT EXISTS (SELECT 1 FROM usermetatbl WHERE login=cmt_login);

ALTER TABLE case_comments ADD COLUMN cmt_userid int4;

UPDATE case_comments SET cmt_userid=usermetatbl.id FROM usermetatbl WHERE login=cmt_login;
DELETE FROM case_comments WHERE cmt IS NULL;
DELETE FROM case_comments WHERE cmt = '';

INSERT INTO "CaseComment" (caseId, ctime, author, comment)
SELECT case_id as caseId, (cmt_ts :: timestamptz) as ctime, cmt_userid as owner, cmt as comment
FROM case_comments;
