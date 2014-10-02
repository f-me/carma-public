ALTER TABLE casetbl DROP COLUMN actions;
DELETE FROM "ConstructorFieldOption" WHERE field='actions';
DELETE FROM "FieldPermission" WHERE field='actions';
