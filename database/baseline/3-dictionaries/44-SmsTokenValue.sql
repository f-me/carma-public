CREATE TABLE "SmsTokenValue"
  ( id    SERIAL PRIMARY KEY
  , token int4 REFERENCES "SmsTokenValue" NOT NULL
  , program int4 REFERENCES "Program" NOT NULL
  , sub_program int4 REFERENCES "SubProgram" NOT NULL
  , value text
  );
