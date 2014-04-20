CREATE TABLE "SmsTokenName"
  ( id    SERIAL PRIMARY KEY
  , label text NOT NULL
  , var_name text UNIQUE NOT NULL
  );
