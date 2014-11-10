
drop table if exists "PartnerService";

create table "PartnerService"
  ( id serial primary key
  , parentId int not null references partnertbl
  , priority1 int
  , priority2 int
  , priority3 int
  , serviceName int not null references "ServiceType" default 1
  , falseCallPercent numeric(5,2)
  );

GRANT ALL ON "PartnerService" TO carma_search;
GRANT ALL ON "PartnerService" TO carma_db_sync;
GRANT ALL ON "PartnerService_id_seq" TO carma_search;
GRANT ALL ON "PartnerService_id_seq" TO carma_db_sync;

insert into "FieldPermission" (role, model, field, r, w)
  select role, 'PartnerService', field, r, w
  from "FieldPermission"
  where model = 'partner_service';

delete from "FieldPermission" where model = 'partner_service';

delete from partner_servicetbl where not exists (select 1 from partnertbl where 'partner:' || id = parentId);

insert into "PartnerService"
  ( id
  , parentId
  , priority1
  , priority2
  , priority3
  , serviceName
  , falseCallPercent
  )
  select
    id,
    regexp_replace(parentId, '\D', '', 'g') :: int,
    nullif(regexp_replace(priority1, '\D', '', 'g'), '') :: int,
    nullif(regexp_replace(priority2, '\D', '', 'g'), '') :: int,
    nullif(regexp_replace(priority3, '\D', '', 'g'), '') :: int,
    serviceName,
    nullif(regexp_replace(falseCallPercent, '\D', '', 'g'), '') :: numeric(5,2)
  from partner_servicetbl;

select setval(pg_get_serial_sequence('"PartnerService"', 'id'), max(id)) from "PartnerService";


update partnertbl set services = replace(services, 'partner_service', 'PartnerService');
