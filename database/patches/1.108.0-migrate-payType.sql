ALTER TABLE servicetbl ADD COLUMN payType_tmp int4;

UPDATE servicetbl SET payType_tmp = 1 WHERE payType = 'ruamc';
UPDATE servicetbl SET payType_tmp = 2 WHERE payType = 'client';
UPDATE servicetbl SET payType_tmp = 3 WHERE payType = 'mixed';
UPDATE servicetbl SET payType_tmp = 4 WHERE payType = 'refund';

ALTER TABLE servicetbl DROP COLUMN payType;
ALTER TABLE tech1tbl DROP COLUMN payType;
ALTER TABLE consultationtbl DROP COLUMN payType;
ALTER TABLE servicetbl ADD COLUMN payType int4 REFERENCES "PaymentType";
UPDATE servicetbl SET payType = payType_tmp;
ALTER TABLE servicetbl DROP COLUMN payType_tmp;
