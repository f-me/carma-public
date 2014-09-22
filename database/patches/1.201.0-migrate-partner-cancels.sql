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

GRANT ALL ON "PartnerCancel" TO carma_db_sync;
GRANT ALL ON "PartnerCancel_id_seq" TO carma_db_sync;


insert into "FieldPermission" (role, model, field, r, w) values
 (1, 'PartnerCancel', 'id', 't', 't')
,(1, 'PartnerCancel', 'ctime', 't', 't')
,(1, 'PartnerCancel', 'caseId', 't', 't')
,(1, 'PartnerCancel', 'partnerId', 't', 't')
,(1, 'PartnerCancel', 'serviceId', 't', 't')
,(1, 'PartnerCancel', 'owner', 't', 't')
,(1, 'PartnerCancel', 'partnerCancelReason', 't', 't')
,(1, 'PartnerCancel', 'comment', 't', 't')
;

insert into "FieldPermission" (role, model, field, r, w)
  select 1, model, field, 't', 'f'
  from "FieldPermission"
  where (model = 'Partner' or model = 'partner_service')
    and role = 3;


delete from partnercanceltbl where partnerid is null;

insert into "PartnerCancel"
  ( id
  , ctime
  , caseId
  , partnerId
  , serviceId
  , owner
  , partnerCancelReason
  , comment
  )
  select
    c.id,
    c.ctime,
    regexp_replace(c.caseId, '\D', '', 'g') :: int,
    regexp_replace(c.partnerId, '\D', '', 'g') :: int,
    c.serviceId,
    usermeta.id,
    refusal.id,
    coalesce(c.comment, '')
  from
    partnercanceltbl c,
    "PartnerRefusalReason" refusal,
    usermetatbl usermeta
  where c.partnerCancelReason = refusal.label
    and c.owner = usermeta.login;
