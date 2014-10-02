DELETE FROM "FieldPermission" WHERE model = 'action' AND field = 'priority';
ALTER TABLE actiontbl DROP COLUMN priority;
