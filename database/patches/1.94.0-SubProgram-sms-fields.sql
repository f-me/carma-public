ALTER TABLE "SubProgram" ADD COLUMN smsSender text
NOT NULL
DEFAULT 'RAMC'
CHECK (smsSender <> '');

ALTER TABLE "SubProgram" ADD COLUMN smsContact text
NOT NULL
DEFAULT '+78002507262'
CHECK (smsContact <> '');

ALTER TABLE "SubProgram" ADD COLUMN smsProgram text
NOT NULL
DEFAULT 'РАМК'
CHECK (smsProgram <> '');
