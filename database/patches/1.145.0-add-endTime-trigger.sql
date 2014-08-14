BEGIN;

ALTER TABLE "UserState" ADD COLUMN "endTime" timestamptz;

-- Fill endTime for existing UserStates
WITH endt AS
(SELECT userid, state, ctime,
     coalesce(lead(ctime) over (partition by userid order by ctime), now())
     as due_time
  FROM "UserState"
  ORDER BY userid)

UPDATE "UserState" SET "endTime" = endt.due_time FROM endt
       WHERE "UserState".userid = endt.userid
       AND "UserState".state = endt.state
       AND "UserState".ctime = endt.ctime;

CREATE OR REPLACE FUNCTION add_end_Time ()
RETURNS trigger AS $$
DECLARE endt timestamp;
BEGIN

UPDATE "UserState" us SET "endTime" = NEW.ctime
  FROM (SELECT id FROM "UserState" WHERE userid = NEW.userid
          ORDER BY id desc LIMIT 1) as s
  WHERE s.id = us.id;
 return NEW;

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS "ADDING_NEW_END_TIME" ON "UserState";
CREATE TRIGGER "ADDING_NEW_END_TIME" BEFORE INSERT
  ON "UserState"
  FOR EACH ROW
  EXECUTE PROCEDURE add_end_Time();

END;
