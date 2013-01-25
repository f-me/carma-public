
ALTER TABLE servicetbl ADD COLUMN urgentService boolean NOT NULL DEFAULT false;
CREATE INDEX ON servicetbl USING hash (urgentService);
CREATE INDEX ON actiontbl USING btree (priority) where closed = false;
GRANT SELECT ON servicetbl TO carma_action_assignment;
