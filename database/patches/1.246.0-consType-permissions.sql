INSERT INTO "FieldPermission" (role, model, field, r, w)
SELECT role, 'ConsultationType', field, r, w
FROM "FieldPermission"
WHERE model = 'PaymentType';
