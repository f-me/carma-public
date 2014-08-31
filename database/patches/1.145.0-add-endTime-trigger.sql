BEGIN;

ALTER TABLE "UserState" ADD COLUMN range tstzrange;

-- Fill endTime for existing UserStates
WITH endt AS
(SELECT id, userid, state, ctime,
     coalesce(lead(ctime) over (partition by userid order by ctime), now())
     as due_time
  FROM "UserState"
  ORDER BY userid)

UPDATE "UserState" us SET range = tstzrange(us.ctime, endt.due_time) FROM endt
       WHERE us.id = endt.id;

CREATE OR REPLACE FUNCTION add_us_range()
RETURNS trigger AS $$
DECLARE endt timestamp;
BEGIN

UPDATE "UserState" us SET range = tstzrange(ctime, NEW.ctime)
  FROM (SELECT id FROM "UserState" WHERE userid = NEW.userid
          ORDER BY id desc LIMIT 1) as s
  WHERE s.id = us.id;
 return NEW;

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS adding_us_range ON "UserState";
CREATE TRIGGER adding_us_range BEFORE INSERT
  ON "UserState"
  FOR EACH ROW
  EXECUTE PROCEDURE add_us_range();

END;
