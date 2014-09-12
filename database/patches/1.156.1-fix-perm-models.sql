UPDATE "FieldPermission" SET model = 'Action' WHERE model = 'action';

DELETE FROM "FieldPermission" WHERE model in ('deliverClient', 'insurance');
