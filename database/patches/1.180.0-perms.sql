DELETE FROM "FieldPermission" WHERE model='Action' and field='serviceType';
UPDATE "FieldPermission" SET w='f' WHERE field='psaExported';
