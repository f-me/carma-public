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
  , makeYear int4
  , carClass int4 REFERENCES "CarClass"
  , color text
  , transmission int4 REFERENCES "Transmission"
  , engineVolume text
  , engineType int4 REFERENCES "Engine"
  , buyDate date
  , seller int4 REFERENCES partnertbl
  , lastCheckDealer int4 REFERENCES partnertbl
  , checkPeriod int4
  , checkType int4 REFERENCES "CheckType"
  , orderNumber text
  , managerName text
  , comment text
  , subprogram  int4 REFERENCES "SubProgram"
  , legalForm  int4 REFERENCES "LegalForm"
  , committer  int4 REFERENCES usermetatbl (id) NOT NULL
  , dixi bool NOT NULL DEFAULT FALSE
  , isActive bool NOT NULL DEFAULT TRUE
  , fromArc bool NOT NULL DEFAULT FALSE
  , extra json
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , sourcefile text NOT NULL DEFAULT ''
  , fts_key text NOT NULL DEFAULT ''
  );


create index on "Contract"(subprogram);
create index on "Contract"
  using gin(fts_key gin_trgm_ops)
  where dixi and isactive;


-- trigger to update fst_key
create or replace function "Contract_fts_key_update" returns trigger as
$$
begin
  new.fts_key = upper(
         coalesce(new.vin, '')        || '\0'
      || coalesce(new.cardNumber, '') || '\0'
      || coalesce(new.plateNum, '')   || '\0'
      || coalesce(new.name, '')       || '\0'
      || coalesce(new.phone, '')      || '\0'
      || coalesce(new.codeword, '')   || '\0'
      || coalesce(new.email, ''));
  return new;
end;
$$ language 'plpgsql';

create trigger "Contract_fts_trigger"
  before insert or update on "Contract"
  for each row
  execute procedure "Contract_fts_key_update"();


GRANT ALL ON "Contract" TO carma_db_sync;
GRANT ALL ON "Contract_id_seq" TO carma_db_sync;
