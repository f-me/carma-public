ALTER TABLE servicetbl RENAME COLUMN urgentService TO urgentService_bkup;
ALTER TABLE servicetbl ADD COLUMN urgentService text;
UPDATE servicetbl SET urgentService = 'priority1' WHERE urgentService_bkup = true;
ALTER TABLE servicetbl DROP COLUMN IF EXISTS urgentService_bkup;

CREATE INDEX ON servicetbl USING hash (urgentService);
CREATE INDEX ON actiontbl USING btree (priority) where closed = false;
GRANT SELECT ON servicetbl TO carma_action_assignment;
