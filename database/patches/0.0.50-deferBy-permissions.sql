INSERT INTO "FieldPermission"
(role, model, field, r, w) 
SELECT role, model, 'deferBy', r, w
FROM "FieldPermission"
WHERE model='action'
AND field='result';
