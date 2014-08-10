-- remap ActionNames to new ActionType idents
ALTER TABLE actiontbl ADD COLUMN type_tmp int4;
UPDATE actiontbl SET type_tmp = 16 WHERE name='accountCheck';
UPDATE actiontbl SET type_tmp = 12 WHERE name='addBill';
UPDATE actiontbl SET type_tmp = 17 WHERE name='analystCheck';
UPDATE actiontbl SET type_tmp = 20 WHERE name='callMeMaybe';
UPDATE actiontbl SET type_tmp = 9 WHERE name='cancelService';
UPDATE actiontbl SET type_tmp = 10 WHERE name='carmakerApproval';
UPDATE actiontbl SET type_tmp = 6 WHERE name='checkEndOfService';
UPDATE actiontbl SET type_tmp = 4 WHERE name='checkStatus';
UPDATE actiontbl SET type_tmp = 7 WHERE name='closeCase';
UPDATE actiontbl SET type_tmp = 18 WHERE name='complaintResolution';
UPDATE actiontbl SET type_tmp = 15 WHERE name='directorCheck';
UPDATE actiontbl SET type_tmp = 8 WHERE name='getInfoDealerVW';
UPDATE actiontbl SET type_tmp = 14 WHERE name='headCheck';
UPDATE actiontbl SET type_tmp = 5 WHERE name='needPartner';
UPDATE actiontbl SET type_tmp = 1 WHERE name='orderService';
UPDATE actiontbl SET type_tmp = 2 WHERE name='orderServiceAnalyst';
UPDATE actiontbl SET type_tmp = 3 WHERE name='tellClient';
UPDATE actiontbl SET type_tmp = 11 WHERE name='tellDealerDenied';
UPDATE actiontbl SET type_tmp = 19 WHERE name='tellMeMore';
-- archive by default
UPDATE actiontbl SET type_tmp = 9000 WHERE type_tmp IS NULL;;
ALTER TABLE actiontbl DROP COLUMN name;
ALTER TABLE actiontbl ADD COLUMN type int4
REFERENCES "ActionType";
UPDATE "FieldPermission" SET field = 'type' WHERE
field = 'name' AND model = 'action';
UPDATE actiontbl SET type = type_tmp;
ALTER TABLE actiontbl ALTER COLUMN type SET NOT NULL;
ALTER TABLE actiontbl DROP COLUMN type_tmp;

-- Fix targetGroup type
ALTER TABLE actiontbl ADD COLUMN targetGroup_tmp int4;
UPDATE actiontbl SET targetGroup = regexp_replace(targetgroup, '\D', '', 'g');
UPDATE actiontbl SET targetGroup = NULL WHERE targetGroup = '';
UPDATE actiontbl SET targetGroup_tmp = targetGroup :: int4;
-- drop broken actions
DELETE FROM actiontbl WHERE targetGroup_tmp IS NULL;
DELETE FROM actiontbl WHERE NOT EXISTS
(SELECT 1 FROM "Role" WHERE id = targetGroup_tmp);
ALTER TABLE actiontbl DROP COLUMN targetGroup;
ALTER TABLE actiontbl ADD COLUMN targetGroup int4 REFERENCES "Role";
UPDATE actiontbl SET targetGroup = targetGroup_tmp;
ALTER TABLE actiontbl ALTER COLUMN targetGroup SET NOT NULL;
ALTER TABLE actiontbl DROP COLUMN targetGroup_tmp;

-- Fix caseId type
ALTER TABLE actiontbl ADD COLUMN caseId_tmp int4;
-- delete broken actions and orphans
DELETE FROM actiontbl
WHERE length(regexp_replace(split_part(caseId,':',2), '\D', '', 'g')) = 0;
UPDATE actiontbl SET caseId_tmp = split_part(caseId,':',2)::int;
DELETE FROM actiontbl WHERE NOT EXISTS
(SELECT 1 FROM casetbl WHERE id = caseId_tmp);
ALTER TABLE actiontbl DROP COLUMN caseId;
ALTER TABLE actiontbl ADD COLUMN caseId int4 REFERENCES casetbl;
UPDATE actiontbl SET caseId = caseId_tmp;
ALTER TABLE actiontbl ALTER COLUMN caseId SET NOT NULL;
ALTER TABLE actiontbl DROP COLUMN caseId_tmp;

-- Remap result to ActionResult idents
ALTER TABLE actiontbl ADD COLUMN result_tmp int4;
UPDATE actiontbl SET result_tmp = 1 WHERE result = 'serviceOrdered';
UPDATE actiontbl SET result_tmp = 10 WHERE result = 'prescheduleService';
UPDATE actiontbl SET result_tmp = 10 WHERE result = 'serviceFinished';
UPDATE actiontbl SET result_tmp = 11 WHERE result = 'caseClosed';
UPDATE actiontbl SET result_tmp = 12 WHERE result = 'vwclosed';
UPDATE actiontbl SET result_tmp = 13 WHERE result = 'carmakerApproved';
UPDATE actiontbl SET result_tmp = 16 WHERE result = 'billAttached';
UPDATE actiontbl SET result_tmp = 17 WHERE result = 'moveToBack';
UPDATE actiontbl SET result_tmp = 17 WHERE result = 'parguyToBack';
UPDATE actiontbl SET result_tmp = 19 WHERE result = 'confirmFinal';
UPDATE actiontbl SET result_tmp = 19 WHERE result = 'dirConfirmFinal';
UPDATE actiontbl SET result_tmp = 2 WHERE result = 'serviceOrderedSMS';
UPDATE actiontbl SET result_tmp = 20 WHERE result = 'confirmWODirector';
UPDATE actiontbl SET result_tmp = 21 WHERE result = 'confirm';
UPDATE actiontbl SET result_tmp = 23 WHERE result = 'directorConfirm';
UPDATE actiontbl SET result_tmp = 24 WHERE result = 'accountToDirector';
UPDATE actiontbl SET result_tmp = 25 WHERE result = 'accountConfirm';
UPDATE actiontbl SET result_tmp = 26 WHERE result = 'analystChecked';
UPDATE actiontbl SET result_tmp = 27 WHERE result = 'complaintManaged';
UPDATE actiontbl SET result_tmp = 28 WHERE result = 'falseCallWBill';
UPDATE actiontbl SET result_tmp = 29 WHERE result = 'falseCallWOBill';
UPDATE actiontbl SET result_tmp = 3 WHERE result = 'needPartner';
UPDATE actiontbl SET result_tmp = 30 WHERE result = 'communicated';
UPDATE actiontbl SET result_tmp = 31 WHERE result = 'okButNoService';
UPDATE actiontbl SET result_tmp = 4 WHERE result = 'clientCanceledService';
UPDATE actiontbl SET result_tmp = 5 WHERE result = 'serviceOrderedAnalyst';
UPDATE actiontbl SET result_tmp = 7 WHERE result = 'partnerOk';
UPDATE actiontbl SET result_tmp = 8 WHERE result = 'serviceInProgress';
UPDATE actiontbl SET result_tmp = 8 WHERE result = 'serviceStillInProgress';
UPDATE actiontbl SET result_tmp = 9 WHERE result = 'partnerFound';
-- archive by default
UPDATE actiontbl SET result_tmp = 9000 WHERE length(result) > 1 AND result_tmp IS NULL;
ALTER TABLE actiontbl DROP COLUMN result;
ALTER TABLE actiontbl ADD COLUMN result int4
REFERENCES "ActionResult";
UPDATE actiontbl SET result = result_tmp;
ALTER TABLE actiontbl DROP COLUMN result_tmp;

-- Assign by id, not login
ALTER TABLE actiontbl ADD COLUMN ass_tmp int4;
UPDATE actiontbl SET ass_tmp = u.id
FROM usermetatbl u WHERE u.login = assignedTo;
ALTER TABLE actiontbl DROP COLUMN assignedTo;
ALTER TABLE actiontbl ADD COLUMN assignedTo int4 REFERENCES usermetatbl (id);
UPDATE actiontbl SET assignedTo = ass_tmp;
ALTER TABLE actiontbl DROP COLUMN ass_tmp;

-- NOT NULL closed
UPDATE actiontbl SET closed = 't' WHERE closed IS NULL;
ALTER TABLE actiontbl ALTER COLUMN closed SET NOT NULL, ALTER COLUMN closed SET DEFAULT 'f';
