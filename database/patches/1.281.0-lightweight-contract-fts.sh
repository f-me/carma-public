#!/bin/bash -e

$PSQL <<EOF

create table "Contract_x"
  ( id SERIAL PRIMARY KEY
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
  , subprogram int4 REFERENCES "SubProgram"
  , legalForm int4 REFERENCES "LegalForm"
  , committer int4 REFERENCES usermetatbl (id) NOT NULL
  , dixi bool NOT NULL DEFAULT FALSE
  , isActive bool NOT NULL DEFAULT TRUE
  , fromArc bool NOT NULL DEFAULT FALSE
  , extra json
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , sourcefile text NOT NULL DEFAULT ''
  , fts_key text NOT NULL DEFAULT ''
  );

insert into "Contract_x"
  ( id
  , name
  , email
  , vin
  , cardNumber
  , codeWord
  , phone
  , plateNum
  , validSince
  , validUntil
  , startMileage
  , make
  , model
  , makeYear
  , carClass
  , color
  , transmission
  , engineVolume
  , engineType
  , buyDate
  , seller
  , lastCheckDealer
  , checkPeriod
  , checkType
  , orderNumber
  , managerName
  , comment
  , subprogram
  , legalForm
  , committer
  , dixi
  , isActive
  , fromArc
  , extra
  , ctime
  , sourcefile
  , fts_key
  )
  select
    id,
    name,
    email,
    vin,
    cardNumber,
    codeWord,
    phone,
    plateNum,
    validSince,
    validUntil,
    startMileage,
    make,
    model,
    makeYear,
    carClass,
    color,
    transmission,
    engineVolume,
    engineType,
    buyDate,
    seller,
    lastCheckDealer,
    checkPeriod,
    checkType,
    orderNumber,
    managerName,
    comment,
    subprogram,
    legalForm,
    committer,
    dixi,
    isActive,
    fromArc,
    extra,
    ctime,
    sourcefile,
    upper(
         coalesce(vin, '')        || '\0'
      || coalesce(cardNumber, '') || '\0'
      || coalesce(plateNum, '')   || '\0'
      || coalesce(name, '')       || '\0'
      || coalesce(phone, '')      || '\0'
      || coalesce(codeword, '')   || '\0'
      || coalesce(email, ''))
  from "Contract";

alter table casetbl drop constraint casetbl_contract_fkey;
drop view if exists "Contract_csv";
drop view if exists "Услуги";
drop view if exists "Контракты";
drop table "Contract";
alter table "Contract_x" rename to "Contract";


alter table casetbl add foreign key (contract) references "Contract"(id);

create index on "Contract"(subprogram);
create index on "Contract"
  using gin(fts_key gin_trgm_ops)
  where dixi and isactive;

create or replace function Contract_fts_key_update() returns trigger as
\$\$
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
\$\$ language 'plpgsql';

create trigger "Contract_fts_trigger"
  before insert or update on "Contract"
  for each row
  execute procedure Contract_fts_key_update();

select setval(pg_get_serial_sequence('"Contract"', 'id'), max(id)) from "Contract";

grant all on "Contract" to carma_db_sync;
grant all on "Contract_x_id_seq" to carma_db_sync;
EOF

$PSQL -f baseline/5-views/6-ru-contracts.sql
$PSQL -f baseline/5-views/7-ru-services.sql
$PSQL -f baseline/5-views/9-csv-contracts.sql
