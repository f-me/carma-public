UPDATE servicetbl SET status = 2 WHERE status IS NULL;
ALTER TABLE servicetbl ALTER COLUMN status SET NOT NULL DEFAULT 2;
