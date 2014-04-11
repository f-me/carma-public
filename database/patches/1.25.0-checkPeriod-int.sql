ALTER TABLE "Contract" DROP COLUMN checkPeriod;
ALTER TABLE "Contract" ADD COLUMN checkPeriod int4;

ALTER TABLE "VinFormat" DROP COLUMN checkPeriodDefault;
ALTER TABLE "VinFormat" ADD COLUMN checkPeriodDefault int4;
