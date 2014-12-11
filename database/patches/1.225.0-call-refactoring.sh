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

ALTER TABLE calltbl ALTER COLUMN callType SET DATA TYPE int4 USING NULL;
ALTER TABLE calltbl ADD CONSTRAINT callTypefk
  FOREIGN KEY (callType) REFERENCES "CallType";

ALTER TABLE calltbl ALTER COLUMN callerType SET DATA TYPE int4 USING NULL;
ALTER TABLE calltbl ADD CONSTRAINT callerTypefk
  FOREIGN KEY (callerType) REFERENCES "CallerType";

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
