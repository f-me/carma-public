ALTER TABLE averagecommissionertbl ADD COLUMN activity_tmp int4 REFERENCES "Activity";

UPDATE averagecommissionertbl SET activity_tmp = 1 WHERE activity = 'callTransfer';
UPDATE averagecommissionertbl SET activity_tmp = 2 WHERE activity = 'conferencing';
UPDATE averagecommissionertbl SET activity_tmp = 3 WHERE activity = 'departure';

ALTER TABLE averagecommissionertbl DROP COLUMN activity;
ALTER TABLE averagecommissionertbl ADD COLUMN activity int4 REFERENCES "Activity";
UPDATE averagecommissionertbl SET activity = activity_tmp;
ALTER TABLE averagecommissionertbl DROP COLUMN activity_tmp;


ALTER TABLE banktbl ADD COLUMN activity_tmp int4 REFERENCES "Activity";

UPDATE banktbl SET activity_tmp = 1 WHERE activity = 'callTransfer';
UPDATE banktbl SET activity_tmp = 2 WHERE activity = 'conferencing';
UPDATE banktbl SET activity_tmp = 3 WHERE activity = 'departure';

ALTER TABLE banktbl DROP COLUMN activity;
ALTER TABLE banktbl ADD COLUMN activity int4 REFERENCES "Activity";
UPDATE banktbl SET activity = activity_tmp;
ALTER TABLE banktbl DROP COLUMN activity_tmp;


ALTER TABLE kentbl ADD COLUMN activity_tmp int4 REFERENCES "Activity";

UPDATE kentbl SET activity_tmp = 1 WHERE activity = 'callTransfer';
UPDATE kentbl SET activity_tmp = 2 WHERE activity = 'conferencing';
UPDATE kentbl SET activity_tmp = 3 WHERE activity = 'departure';

ALTER TABLE kentbl DROP COLUMN activity;
ALTER TABLE kentbl ADD COLUMN activity int4 REFERENCES "Activity";
UPDATE kentbl SET activity = activity_tmp;
ALTER TABLE kentbl DROP COLUMN activity_tmp;


ALTER TABLE tech1tbl ADD COLUMN activity_tmp int4 REFERENCES "Activity";

UPDATE tech1tbl SET activity_tmp = 1 WHERE activity = 'callTransfer';
UPDATE tech1tbl SET activity_tmp = 2 WHERE activity = 'conferencing';
UPDATE tech1tbl SET activity_tmp = 3 WHERE activity = 'departure';

ALTER TABLE tech1tbl DROP COLUMN activity;
ALTER TABLE tech1tbl ADD COLUMN activity int4 REFERENCES "Activity";
UPDATE tech1tbl SET activity = activity_tmp;
ALTER TABLE tech1tbl DROP COLUMN activity_tmp;
