DELETE FROM partnertbl WHERE name IS NULL;

ALTER TABLE partnertbl ALTER name SET NOT NULL;
