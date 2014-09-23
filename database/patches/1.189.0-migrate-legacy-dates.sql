ALTER TABLE casetbl
ALTER COLUMN repair
TYPE date USING ((repair + '4 hours') at time zone 'utc');

ALTER TABLE servicetbl
ALTER COLUMN bill_billingDate
TYPE date USING ((bill_billingDate + '4 hours') at time zone 'utc');

ALTER TABLE towagetbl
ALTER COLUMN repairenddate
TYPE date USING ((repairenddate + '4 hours') at time zone 'utc');
