UPDATE attachmenttbl SET hash = '' WHERE hash IS NULL;
ALTER TABLE attachmenttbl ALTER COLUMN hash SET NOT NULL;

UPDATE "Region" SET cities = '{}' WHERE cities IS NULL;
ALTER TABLE "Region" ALTER COLUMN cities SET NOT NULL;
