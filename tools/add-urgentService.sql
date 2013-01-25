
ALTER TABLE servicetbl ADD COLUMN urgentService boolean NOT NULL DEFAULT false;
CREATE INDEX ON servicetbl USING hash (urgentService);
