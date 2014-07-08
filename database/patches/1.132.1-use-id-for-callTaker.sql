BEGIN;
-- drop views that depend on calltaker
DROP VIEW "Звонки";
DROP VIEW servicesview;
DROP VIEW "Услуги";

-- update call table
ALTER TABLE calltbl ADD COLUMN calltakerid int4 references usermetatbl(id);
UPDATE calltbl c SET calltakerid = u.id
       FROM usermetatbl u
       WHERE u.login = c.calltaker;

-- assign calls with unknown calltakers to admin
UPDATE calltbl SET calltakerid = 90 WHERE calltakerid IS NULL;

ALTER TABLE calltbl ALTER COLUMN calltakerid SET NOT NULL;
ALTER TABLE calltbl DROP COLUMN calltaker;
ALTER TABLE calltbl RENAME COLUMN calltakerid to calltaker;
CREATE INDEX call_calltaker_idx ON calltbl (calltaker);

-- update case table
ALTER TABLE casetbl ADD COLUMN calltakerid int4 references usermetatbl(id);
UPDATE casetbl c SET calltakerid = u.id
       FROM usermetatbl u
       WHERE u.realname = c.calltaker;

-- assign calls with unknown calltakers to admin
UPDATE casetbl SET calltakerid = 90 WHERE calltakerid IS NULL;

ALTER TABLE casetbl ALTER COLUMN calltakerid SET NOT NULL;
ALTER TABLE casetbl DROP COLUMN calltaker;
ALTER TABLE casetbl RENAME COLUMN calltakerid to calltaker;


CREATE INDEX case_calltaker_idx ON casetbl (calltaker);

END;
