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
  );

GRANT ALL ON "UserState" TO carma_db_sync;
GRANT ALL ON "UserState_id_seq" TO carma_db_sync;
