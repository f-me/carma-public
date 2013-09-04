INSERT INTO "FieldPermission"
(role, model, field, r, w) 
SELECT 'bo_control', model,field, r, w
FROM "FieldPermission"
WHERE role='back';
