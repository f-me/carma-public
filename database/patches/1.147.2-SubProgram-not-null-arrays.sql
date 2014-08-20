UPDATE "SubProgram" SET contacts = '{}' WHERE contacts IS NULL;
UPDATE "SubProgram" SET services = '{}' WHERE services IS NULL;
UPDATE "SubProgram" SET contractPermissions = '{}' WHERE contractPermissions IS NULL;

ALTER TABLE "SubProgram" ALTER COLUMN contacts SET DEFAULT '{}',
ALTER COLUMN contacts SET NOT NULL;
ALTER TABLE "SubProgram" ALTER COLUMN services SET DEFAULT '{}',
ALTER COLUMN services SET NOT NULL;
ALTER TABLE "SubProgram" ALTER COLUMN contractPermissions SET DEFAULT '{}',
ALTER COLUMN contractPermissions SET NOT NULL;
