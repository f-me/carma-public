drop table if exists "PartnerCancel";

create table "PartnerCancel"
  ( id serial primary key
  , ctime timestamptz not null default now()
  , caseId int not null references casetbl
  , partnerId int not null references partnertbl
  , serviceId int not null -- FIXME: references servicetbl
  , owner int not null references usermetatbl
  , partnerCancelReason int not null references "PartnerRefusalReason"
  , comment text not null default ''
  );

CREATE INDEX ON "PartnerCancel"(caseId);

GRANT ALL ON "PartnerCancel" TO carma_db_sync;
GRANT ALL ON "PartnerCancel_id_seq" TO carma_db_sync;
