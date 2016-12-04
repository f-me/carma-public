CREATE TYPE "UserStateVal"
  AS ENUM( 'LoggedOut'
         , 'Ready'
         , 'Rest'
         , 'Busy'
         , 'Dinner'
         , 'ServiceBreak'
         , 'NA');

CREATE TABLE "UserState"
  (id      SERIAL PRIMARY KEY
  ,ctime   timestamptz NOT NULL DEFAULT now()
  ,state   "UserStateVal" NOT NULL
  ,eventId int4 REFERENCES "Event" (id)
  ,userId  int4 REFERENCES "usermetatbl" (id)
  ,latest  boolean NOT NULL DEFAULT true
  );



CREATE OR REPLACE FUNCTION add_us_range() RETURNS TRIGGER AS
$$
  DECLARE endt timestamp;
  BEGIN
  UPDATE "UserState" us
    SET range = tstzrange(ctime, NEW.ctime)
    FROM (SELECT id FROM "UserState" WHERE userid = NEW.userid
            ORDER BY id desc LIMIT 1) as s
    WHERE s.id = us.id;
   return NEW;
  END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER adding_us_range
  BEFORE INSERT ON "UserState"
  FOR EACH ROW
  EXECUTE PROCEDURE add_us_range();

CREATE INDEX ON "UserState"(userId) where range is null;



GRANT ALL ON "UserState" TO carma_db_sync;
GRANT ALL ON "UserState_id_seq" TO carma_db_sync;
