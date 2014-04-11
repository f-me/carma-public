UPDATE usermetatbl SET roles = array_remove(roles, '33') WHERE roles && '{33}';
DELETE FROM "FieldPermission" WHERE role = 33;
DELETE FROM "Role" WHERE id = 33;
UPDATE "Role" SET label = 'Оператор Front Office' WHERE id = 2;
