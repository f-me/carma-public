ALTER TABLE deliverclienttbl ADD COLUMN deliveryType_tmp int4 REFERENCES "RequestType";

UPDATE deliverclienttbl SET deliveryType_tmp = 1 WHERE deliveryType = 'air';
UPDATE deliverclienttbl SET deliveryType_tmp = 2 WHERE deliveryType = 'train';
UPDATE deliverclienttbl SET deliveryType_tmp = 3 WHERE deliveryType = 'boat';
UPDATE deliverclienttbl SET deliveryType_tmp = 4 WHERE deliveryType = 'bus';

ALTER TABLE deliverclienttbl DROP COLUMN deliveryType;
ALTER TABLE deliverclienttbl ADD COLUMN deliveryType int4 REFERENCES "RequestType";
UPDATE deliverclienttbl SET deliveryType = deliveryType_tmp;
ALTER TABLE deliverclienttbl DROP COLUMN deliveryType_tmp;


ALTER TABLE continuetbl ADD COLUMN deliveryType_tmp int4 REFERENCES "RequestType";

UPDATE continuetbl SET deliveryType_tmp = 1 WHERE deliveryType = 'air';
UPDATE continuetbl SET deliveryType_tmp = 2 WHERE deliveryType = 'train';
UPDATE continuetbl SET deliveryType_tmp = 3 WHERE deliveryType = 'boat';
UPDATE continuetbl SET deliveryType_tmp = 4 WHERE deliveryType = 'bus';

ALTER TABLE continuetbl DROP COLUMN deliveryType;
ALTER TABLE continuetbl ADD COLUMN deliveryType int4 REFERENCES "RequestType";
UPDATE continuetbl SET deliveryType = deliveryType_tmp;
ALTER TABLE continuetbl DROP COLUMN deliveryType_tmp;


ALTER TABLE ticketstbl ADD COLUMN deliveryType_tmp int4 REFERENCES "RequestType";

UPDATE ticketstbl SET deliveryType_tmp = 1 WHERE deliveryType = 'air';
UPDATE ticketstbl SET deliveryType_tmp = 2 WHERE deliveryType = 'train';
UPDATE ticketstbl SET deliveryType_tmp = 3 WHERE deliveryType = 'boat';
UPDATE ticketstbl SET deliveryType_tmp = 4 WHERE deliveryType = 'bus';

ALTER TABLE ticketstbl DROP COLUMN deliveryType;
ALTER TABLE ticketstbl ADD COLUMN deliveryType int4 REFERENCES "RequestType";
UPDATE ticketstbl SET deliveryType = deliveryType_tmp;
ALTER TABLE ticketstbl DROP COLUMN deliveryType_tmp;
