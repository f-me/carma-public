CREATE SEQUENCE attachmenttbl_id_seq;
SELECT setval('attachmenttbl_id_seq', max(id)) FROM attachmenttbl;
ALTER TABLE attachmenttbl ALTER COLUMN id SET DEFAULT nextval('attachmenttbl_id_seq');
GRANT ALL ON "attachmenttbl_id_seq" TO carma_db_sync;
GRANT ALL ON "attachmenttbl_id_seq" TO carma_search;

ALTER TABLE attachmenttbl
ADD CONSTRAINT "attachmenttbl_filename_nonempty"
CHECK (filename <> '');

DELETE FROM attachmenttbl WHERE filename IS NULL;

ALTER TABLE attachmenttbl
ALTER COLUMN filename SET NOT NULL;

UPDATE attachmenttbl SET ctime = '2013-09-01' WHERE ctime IS NULL;

ALTER TABLE attachmenttbl ALTER COLUMN ctime SET NOT NULL;

UPDATE "FieldPermission" SET model='Attachment' WHERE model='attachment';

UPDATE casetbl SET files=regexp_replace(files, 'attachment', 'Attachment', 'g');
UPDATE servicetbl SET files=regexp_replace(files, 'attachment', 'Attachment', 'g');
UPDATE "SubProgram" SET logo=regexp_replace(logo, 'attachment', 'Attachment', 'g');
UPDATE "SubProgram" SET template=regexp_replace(template, 'attachment', 'Attachment', 'g');
