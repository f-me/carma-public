ALTER TABLE averagecommissionertbl ADD COLUMN requestType_tmp int4 REFERENCES "RequestType";

UPDATE averagecommissionertbl SET requestType_tmp = 1 WHERE requestType = 'cons';
UPDATE averagecommissionertbl SET requestType_tmp = 2 WHERE requestType = 'first';
UPDATE averagecommissionertbl SET requestType_tmp = 3 WHERE requestType = 'evaluation';
UPDATE averagecommissionertbl SET requestType_tmp = 4 WHERE requestType = 'rep';

ALTER TABLE averagecommissionertbl DROP COLUMN requestType;
ALTER TABLE averagecommissionertbl ADD COLUMN requestType int4 REFERENCES "RequestType";
UPDATE averagecommissionertbl SET requestType = requestType_tmp;
ALTER TABLE averagecommissionertbl DROP COLUMN requestType_tmp;


ALTER TABLE banktbl ADD COLUMN requestType_tmp int4 REFERENCES "RequestType";

UPDATE banktbl SET requestType_tmp = 1 WHERE requestType = 'cons';
UPDATE banktbl SET requestType_tmp = 2 WHERE requestType = 'first';
UPDATE banktbl SET requestType_tmp = 3 WHERE requestType = 'evaluation';
UPDATE banktbl SET requestType_tmp = 4 WHERE requestType = 'rep';

ALTER TABLE banktbl DROP COLUMN requestType;
ALTER TABLE banktbl ADD COLUMN requestType int4 REFERENCES "RequestType";
UPDATE banktbl SET requestType = requestType_tmp;
ALTER TABLE banktbl DROP COLUMN requestType_tmp;


ALTER TABLE kentbl ADD COLUMN requestType_tmp int4 REFERENCES "RequestType";

UPDATE kentbl SET requestType_tmp = 1 WHERE requestType = 'cons';
UPDATE kentbl SET requestType_tmp = 2 WHERE requestType = 'first';
UPDATE kentbl SET requestType_tmp = 3 WHERE requestType = 'evaluation';
UPDATE kentbl SET requestType_tmp = 4 WHERE requestType = 'rep';

ALTER TABLE kentbl DROP COLUMN requestType;
ALTER TABLE kentbl ADD COLUMN requestType int4 REFERENCES "RequestType";
UPDATE kentbl SET requestType = requestType_tmp;
ALTER TABLE kentbl DROP COLUMN requestType_tmp;


ALTER TABLE tech1tbl ADD COLUMN requestType_tmp int4 REFERENCES "RequestType";

UPDATE tech1tbl SET requestType_tmp = 1 WHERE requestType = 'cons';
UPDATE tech1tbl SET requestType_tmp = 2 WHERE requestType = 'first';
UPDATE tech1tbl SET requestType_tmp = 3 WHERE requestType = 'evaluation';
UPDATE tech1tbl SET requestType_tmp = 4 WHERE requestType = 'rep';

ALTER TABLE tech1tbl DROP COLUMN requestType;
ALTER TABLE tech1tbl ADD COLUMN requestType int4 REFERENCES "RequestType";
UPDATE tech1tbl SET requestType = requestType_tmp;
ALTER TABLE tech1tbl DROP COLUMN requestType_tmp;
