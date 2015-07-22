ALTER TABLE towagetbl ADD COLUMN wheelsBlocked int4;

UPDATE towagetbl SET wheelsBlocked = 0 WHERE wheelsUnblocked = 'w0';
UPDATE towagetbl SET wheelsBlocked = 1 WHERE wheelsUnblocked = 'w1';
UPDATE towagetbl SET wheelsBlocked = 2 WHERE wheelsUnblocked = 'w2';
UPDATE towagetbl SET wheelsBlocked = 3 WHERE wheelsUnblocked = 'w3';
UPDATE towagetbl SET wheelsBlocked = 4 WHERE wheelsUnblocked = 'w4';

ALTER TABLE towagetbl DROP COLUMN wheelsUnblocked;

UPDATE "FieldPermission" SET field = 'wheelsBlocked' WHERE field = 'wheelsUnblocked';
UPDATE "ConstructorFieldOption" SET field = 'wheelsBlocked' WHERE field = 'wheelsUnblocked';
