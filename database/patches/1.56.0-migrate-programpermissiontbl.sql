-- Migrate programpermissionstbl to SubProgramContractPermission,
-- using subprogram numeric ids instead of text references.
INSERT INTO "SubProgramContractPermission"
(parent, contractField, showTable, showForm)
SELECT s.id, contractfield, showtable, showform
FROM "SubProgram" s, programpermissionstbl f, programtbl p
WHERE cast(split_part(f.parentid, ':', 2) as integer)=p.id
AND s.value=p.value
AND contractfield IS NOT NULL
AND showtable IS NOT NULL
AND showform IS NOT NULL;

-- Remap old contract field names
UPDATE "SubProgramContractPermission"
SET contractField = 'make' WHERE contractField = 'carMake';

UPDATE "SubProgramContractPermission"
SET contractField='buyDate' WHERE contractField = 'carBuyDate';

UPDATE "SubProgramContractPermission"
SET contractField='checkPeriod' WHERE contractField='carCheckPeriod';

UPDATE "SubProgramContractPermission"
SET contractField='color' WHERE contractField='carColor';

UPDATE "SubProgramContractPermission"
SET contractField='lastCheckDealer' WHERE contractField='carDealerTO';

UPDATE "SubProgramContractPermission"
SET contractField='cardNumber' WHERE contractField='cardNumber';

UPDATE "SubProgramContractPermission"
SET contractField='name' WHERE contractField='cardOwner';

UPDATE "SubProgramContractPermission"
SET contractField='make' WHERE contractField='carMake';

UPDATE "SubProgramContractPermission"
SET contractField='makeYear' WHERE contractField='carMakeYear';

UPDATE "SubProgramContractPermission"
SET contractField='model' WHERE contractField='carModel';

UPDATE "SubProgramContractPermission"
SET contractField='plateNum' WHERE contractField='carPlateNum';

UPDATE "SubProgramContractPermission"
SET contractField='seller' WHERE contractField='carSeller';

UPDATE "SubProgramContractPermission"
SET contractField='transmission' WHERE contractField='carTransmission';

UPDATE "SubProgramContractPermission"
SET contractField='vin' WHERE contractField='carVin';

UPDATE "SubProgramContractPermission"
SET contractField='comment' WHERE contractField='comment';

UPDATE "SubProgramContractPermission"
SET contractField='validSince' WHERE contractField='contractValidFromDate';

UPDATE "SubProgramContractPermission"
SET contractField='validUntil' WHERE contractField='contractValidUntilDate';

UPDATE "SubProgramContractPermission"
SET contractField='validSince' WHERE contractField='warrantyStart';

UPDATE "SubProgramContractPermission"
SET contractField='validUntil' WHERE contractField='warrantyEnd';

UPDATE "SubProgramContractPermission"
SET contractField='isActive' WHERE contractField='isActive';

UPDATE "SubProgramContractPermission"
SET contractField='managerName' WHERE contractField='manager';

UPDATE "SubProgramContractPermission"
SET contractField='startMileage' WHERE contractField='milageTO';

UPDATE "SubProgramContractPermission"
SET contractField='committer' WHERE contractField='owner';

UPDATE "SubProgramContractPermission"
SET contractField='subprogram' WHERE contractField='program';

UPDATE "SubProgramContractPermission"
SET contractField='checkType' WHERE contractField='techType';

-- Delete obsolete field permissions
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'carCheckupDate';
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'carEngine';
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'carCheckupMilage';
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'orderNumber';
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'contractType';
DELETE FROM "SubProgramContractPermission" WHERE contractField = 'contractValidUntilMilage';

-- Update forward links
WITH cps AS
(SELECT array_agg(id) as ids, parent
 FROM "SubProgramContractPermission" GROUP BY parent)
UPDATE "SubProgram" SET contractPermissions = ids
FROM cps WHERE id = cps.parent;
