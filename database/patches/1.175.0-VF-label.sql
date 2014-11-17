DELETE FROM "VinFormat" where label='';

ALTER TABLE "VinFormat"
ADD CONSTRAINT "VinFormat_label_nonempty"
CHECK (label <> '');
