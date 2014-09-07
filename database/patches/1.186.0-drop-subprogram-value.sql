ALTER TABLE "SubProgram" DROP COLUMN value;

DELETE FROM "FieldPermission" WHERE field='value'
AND model IN ('Colors', 'SubProgram');
