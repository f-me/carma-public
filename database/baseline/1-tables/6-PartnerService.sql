
drop table if exists "PartnerService";

create table "PartnerService"
  ( id serial primary key
  , parentId int not null references partnertbl
  , priority1 int
  , priority2 int
  , priority3 int
  , serviceName int not null references "ServiceType"
  , falseCallPercent numeric(5,2)
  );

GRANT ALL ON "PartnerService" TO carma_search;
GRANT ALL ON "PartnerService_id_seq" TO carma_search;
