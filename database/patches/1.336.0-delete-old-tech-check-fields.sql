BEGIN;

DELETE FROM "ConstructorFieldOption" WHERE field LIKE 'check%' AND model=14;
DELETE FROM "FieldPermission" WHERE field LIKE 'check%' AND model='Tech';

COMMIT;
