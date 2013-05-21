CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "ActionName"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('ActionName');
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "ActionResult"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "ActionName" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "ActionResult" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'ActionResult', id
    FROM Dictionary WHERE name = 'ActionName';
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "CallerType"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('CallerType');
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "CallType"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "CallerType" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "CallType" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'CallType', id
    FROM Dictionary WHERE name = 'CallerType';
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "CarMaker"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('CarMaker');
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "CarModel"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "CarMaker" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "CarModel" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'CarModel', id
    FROM Dictionary WHERE name = 'CarMaker';
CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );


CREATE TABLE "City"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('City');
