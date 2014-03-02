UPDATE calltbl SET callDate = '1970-01-01' where callDate is null;
UPDATE calltbl SET callTaker = ''          where callTaker is null;

ALTER TABLE calltbl
      ALTER COLUMN id SET NOT NULL,
      ALTER COLUMN callDate  SET NOT NULL,
      ALTER COLUMN callDate  SET DEFAULT now(),
      ALTER COLUMN callTaker SET NOT NULL;
