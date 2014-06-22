WITH leaders AS (SELECT DISTINCT ON (parent) id FROM "SubProgram" ORDER BY parent,id)
UPDATE "SubProgram" s SET leader = 'true' WHERE EXISTS (SELECT 1 FROM leaders l WHERE l.id = s.id);
