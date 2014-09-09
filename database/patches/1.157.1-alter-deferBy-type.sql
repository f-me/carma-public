-- New deferred actions will always be closed. To avoid confusion, we
-- clean deferBy for all old actions.
ALTER TABLE actiontbl ALTER COLUMN deferBy TYPE interval USING(null);
