CREATE TEMPORARY TABLE "FieldPermission_tmp"
  (role  text
  ,model text
  ,field text
  ,r     bool
  ,w     bool
  ,UNIQUE (role,model,field)
  );

CREATE TABLE "FieldPermission"
  (id    SERIAL PRIMARY KEY
  ,role  int4 REFERENCES "Role"
  ,model text
  ,field text
  ,r     bool
  ,w     bool
  ,UNIQUE (role,model,field)
  );

GRANT ALL ON "FieldPermission" TO carma_search;
GRANT ALL ON "FieldPermission" TO carma_db_sync;
