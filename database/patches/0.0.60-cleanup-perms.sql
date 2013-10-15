DELETE FROM "FieldPermission" WHERE id IN 
(SELECT id FROM 
 (SELECT COUNT(*) AS q, MIN(id) AS id, (role,model,field) FROM "FieldPermission" GROUP BY (role,model,field)) s 
WHERE q > 1);
