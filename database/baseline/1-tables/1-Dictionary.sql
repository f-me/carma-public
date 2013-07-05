CREATE TABLE Dictionary
  (id          SERIAL PRIMARY KEY
  ,name        text UNIQUE NOT NULL
  ,description text
  ,parent      int4 REFERENCES Dictionary
  );
