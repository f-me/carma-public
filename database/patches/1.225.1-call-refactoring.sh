$PSQL <<EOF

BEGIN;

DROP VIEW "Звонки";
DROP TABLE "CallType";
`cat baseline/3-dictionaries/3-CallType.sql`

DROP TABLE "CallerType";
`cat baseline/3-dictionaries/2-CallerType.sql`

`cat baseline/3-dictionaries/43-CallReason.sql`

`cat baseline/3-dictionaries/44-AbuseTarget.sql`

ALTER TABLE calltbl RENAME callerName_name   TO callerName;
ALTER TABLE calltbl RENAME callerName_phone1 TO callerPhone;

ALTER TABLE calltbl DROP COLUMN callerName_phone2;
ALTER TABLE calltbl DROP COLUMN callerName_phone3;
ALTER TABLE calltbl DROP COLUMN callerName_phone4;

ALTER TABLE calltbl DROP COLUMN callername_email;

ALTER TABLE calltbl DROP COLUMN callername_contactowner;
ALTER TABLE calltbl DROP COLUMN callername_ownername;
ALTER TABLE calltbl DROP COLUMN callername_ownerphone1;
ALTER TABLE calltbl DROP COLUMN callername_ownerphone2;
ALTER TABLE calltbl DROP COLUMN callername_ownerphone3;
ALTER TABLE calltbl DROP COLUMN callername_ownerphone4;

ALTER TABLE calltbl DROP COLUMN callername_owneremail;

ALTER TABLE calltbl DROP COLUMN subprogram;
ALTER TABLE calltbl DROP COLUMN wazzup;

ALTER TABLE calltbl DROP COLUMN city;
ALTER TABLE calltbl DROP COLUMN carmake;
ALTER TABLE calltbl DROP COLUMN carmodel;

ALTER TABLE calltbl RENAME COLUMN callType TO callTypeOld;
ALTER TABLE calltbl ADD COLUMN callType
  int4 REFERENCES "CallType" NOT NULL DEFAULT 1;

UPDATE calltbl SET callType = 2 where callTypeOld = 'newCase';
UPDATE calltbl SET callType = 3
  WHERE callTypeOld IN ('processingCase', 'infoCase');

ALTER TABLE calltbl DROP COLUMN callTypeOld;

ALTER TABLE calltbl RENAME COLUMN callerType TO callerTypeOld;

ALTER TABLE calltbl ADD COLUMN callerType int4 REFERENCES "CallerType";

UPDATE calltbl SET callerType = 1 WHERE callerTypeOld = 'client';
UPDATE calltbl SET callerType = 2 WHERE callerTypeOld = 'partner';
UPDATE calltbl SET callerType = 3 WHERE callerTypeOld = 'dealer';
UPDATE calltbl SET callerType = 4 WHERE callerTypeOld = 'staff';
UPDATE calltbl SET callerType = 5 WHERE callerTypeOld = 'other';

ALTER TABLE calltbl DROP COLUMN callerTypeOld;

ALTER TABLE calltbl ADD COLUMN callReason int4 REFERENCES "CallReason";
ALTER TABLE calltbl ADD COLUMN abuseTarget int4 REFERENCES "AbuseTarget";

ALTER TABLE calltbl ADD COLUMN partner int4 REFERENCES partnertbl;

INSERT INTO "FieldPermission" (role, model, field, r, w) VALUES
  (2, 'Call', 'callerName', 't', 't'),
  (2, 'Call', 'callerPhone', 't', 't'),
  (2, 'Call', 'callReason', 't', 't'),
  (2, 'Call', 'abuseTarget', 't', 't'),
  (2, 'Call', 'partner', 't', 't'),

  (17, 'Call', 'callerName', 't', 'f'),
  (17, 'Call', 'callerPhone', 't', 'f'),
  (17, 'Call', 'callReason', 't', 'f'),
  (17, 'Call', 'abuseTarget', 't', 'f'),
  (17, 'Call', 'partner', 't', 'f');

END;

EOF

