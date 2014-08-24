ALTER TABLE casetbl DROP COLUMN actions;
DELETE FROM "ConstructorFieldOption" WHERE field='actions';
DELETE FROM "FieldPermissions" WHERE field='actions';
