CREATE TABLE "Contract"
  ( id    SERIAL PRIMARY KEY
  , name text
  , email text
  , vin text
  , cardNumber text
  , codeWord text
  , phone text
  , plateNum text
  , validSince date
  , validUntil date
  , startMileage int4
  , make int4 REFERENCES "CarMake"
  , model int4 REFERENCES "CarModel"
  , makeYear int2
  , carClass int4 REFERENCES "CarClass"
  , color int4 REFERENCES "Colors"
  , transmission int4 REFERENCES "Transmission"
  , engineVolume float
  , engineType int4 REFERENCES "Engine"
  , buyDate date
  , seller int4 REFERENCES partnertbl
  , lastCheckDealer int4 REFERENCES partnertbl
  , lastCheckMileage int4
  , lastCheckDate date
  , checkPeriod int2
  , checkType int4 REFERENCES "CheckType"
  , orderNumber text
  , managerName text
  , comment text
  , subprogram  int4 REFERENCES "SubProgram"
  , legalForm  int4 REFERENCES "LegalForm"
  , committer  int4 REFERENCES usermetatbl (id) NOT NULL
  , dixi bool NOT NULL DEFAULT TRUE
  , isActive bool NOT NULL DEFAULT TRUE
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  );

GRANT ALL ON "Contract" TO carma_db_sync;
GRANT ALL ON "Contract" TO carma_search;
GRANT ALL ON "Contract_id_seq" TO carma_db_sync;
GRANT ALL ON "Contract_id_seq" TO carma_search;
