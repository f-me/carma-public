-- case.vinChecked

ALTER TABLE casetbl ADD COLUMN ccs_tmp int4;

UPDATE casetbl
SET ccs_tmp = 1
WHERE vinChecked = 'base';

UPDATE casetbl
SET ccs_tmp = 2
WHERE vinChecked = 'dealer';

UPDATE casetbl
SET ccs_tmp = 3
WHERE vinChecked = 'fdds';

UPDATE casetbl
SET ccs_tmp = 4
WHERE vinChecked = 'service';

UPDATE casetbl
SET ccs_tmp = 5
WHERE vinChecked = 'partner';

UPDATE casetbl
SET ccs_tmp = 6
WHERE vinChecked = 'vinNotFound';

UPDATE casetbl
SET ccs_tmp = 7
WHERE vinChecked = 'vinExpired';

UPDATE casetbl
SET ccs_tmp = 8
WHERE vinChecked = 'cardFound';

UPDATE casetbl
SET ccs_tmp = 9
WHERE vinChecked = 'cardNotFound';

ALTER TABLE casetbl DROP COLUMN vinChecked;
ALTER TABLE casetbl ADD COLUMN vinChecked int4 REFERENCES "ContractCheckStatus";
UPDATE casetbl SET vinChecked = ccs_tmp WHERE ccs_tmp IS NOT NULL;
ALTER TABLE casetbl DROP COLUMN ccs_tmp;


-- tech.techType

ALTER TABLE techtbl ADD COLUMN techType_tmp int4;

UPDATE techtbl
SET techType_tmp = 1
WHERE techType = 'fuel';

UPDATE techtbl
SET techType_tmp = 2
WHERE techType = 'oil';

UPDATE techtbl
SET techType_tmp = 3
WHERE techType = 'wheel';

UPDATE techtbl
SET techType_tmp = 4
WHERE techType = 'charge';

UPDATE techtbl
SET techType_tmp = 5
WHERE techType = 'starter';

UPDATE techtbl
SET techType_tmp = 6
WHERE techType = 'condition';

UPDATE techtbl
SET techType_tmp = 7
WHERE techType = 'alarm';

UPDATE techtbl
SET techType_tmp = 8
WHERE techType = 'unfuel';

UPDATE techtbl
SET techType_tmp = 9
WHERE techType = 'unlock';

UPDATE techtbl
SET techType_tmp = 10
WHERE techType = 'unpark';

UPDATE techtbl
SET techType_tmp = 11
WHERE techType = 'reroad';

UPDATE techtbl
SET techType_tmp = 12
WHERE techType = 'wipers';

UPDATE techtbl
SET techType_tmp = 13
WHERE techType = 'relamp';

UPDATE techtbl
SET techType_tmp = 14
WHERE techType = 'elrepair';

UPDATE techtbl
SET techType_tmp = 15
WHERE techType = 'unfreeze';

DROP VIEW allservicesview;

ALTER TABLE techtbl DROP COLUMN techType;
ALTER TABLE techtbl ADD COLUMN techType int4 REFERENCES "TechType";
UPDATE techtbl SET techType = techType_tmp WHERE techType_tmp IS NOT NULL;
ALTER TABLE techtbl DROP COLUMN techType_tmp;


-- towage.towType

ALTER TABLE towagetbl ADD COLUMN towType_tmp int4;

UPDATE towagetbl
SET towType_tmp = 1
WHERE towType = 'dealer';

UPDATE towagetbl
SET towType_tmp = 2
WHERE towType = 'hnight';

UPDATE towagetbl
SET towType_tmp = 3
WHERE towType = 'carshop';

UPDATE towagetbl
SET towType_tmp = 4
WHERE towType = 'other';

ALTER TABLE towagetbl DROP COLUMN towType;
ALTER TABLE towagetbl ADD COLUMN towType int4 REFERENCES "TowType";
UPDATE towagetbl SET towType = towType_tmp WHERE towType_tmp IS NOT NULL;
ALTER TABLE towagetbl DROP COLUMN towType_tmp;
