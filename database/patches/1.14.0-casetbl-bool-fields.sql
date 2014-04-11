-- Migrate nullable bools to non-nullable
ALTER TABLE casetbl ADD COLUMN psaExportNeeded_tmp bool NOT NULL DEFAULT 'f';
ALTER TABLE casetbl ADD COLUMN psaExported_tmp bool NOT NULL DEFAULT 'f';
ALTER TABLE casetbl ADD COLUMN contact_contactOwner_tmp bool NOT NULL DEFAULT 'f';

UPDATE casetbl SET psaExportNeeded_tmp = 't' WHERE psaExportNeeded = 't';
UPDATE casetbl SET psaExported_tmp = 't' WHERE psaExported = 't';
UPDATE casetbl SET contact_contactOwner_tmp = 't' WHERE contact_contactOwner = 't';

ALTER TABLE casetbl DROP COLUMN psaExportNeeded;
ALTER TABLE casetbl DROP COLUMN psaExported;
ALTER TABLE casetbl DROP COLUMN contact_contactOwner;

ALTER TABLE casetbl ADD COLUMN psaExportNeeded bool NOT NULL DEFAULT 'f';
ALTER TABLE casetbl ADD COLUMN psaExported bool NOT NULL DEFAULT 'f';
ALTER TABLE casetbl ADD COLUMN contact_contactOwner bool NOT NULL DEFAULT 'f';

UPDATE casetbl SET psaExportNeeded = psaExportNeeded_tmp;
UPDATE casetbl SET psaExported = psaExported_tmp;
UPDATE casetbl SET contact_contactOwner = contact_contactOwner_tmp;
