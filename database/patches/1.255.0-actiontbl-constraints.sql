UPDATE actiontbl
SET closeTime = coalesce(opentime + '10 minutes', duetime + '30 minutes')
WHERE result IS NOT NULL AND closeTime IS NULL;

ALTER TABLE actiontbl ADD CONSTRAINT "know_closeTime_for_results" CHECK (result IS NULL OR closeTime IS NOT NULL);
