
alter table "Contract" add column registrationReason int references "ContractRegistrationReason"(id);
alter table "Contract" add column priceInOrder numeric(7,2);


insert into "FieldPermission" (role, model, field, r, w) values
  ( 1, 'Contract', 'registrationReason', true, false)
, (18, 'Contract', 'registrationReason', true, false)
, (19, 'Contract', 'registrationReason', true, true)
, (20, 'Contract', 'registrationReason', true, true)
, ( 1, 'Contract', 'priceInOrder',       true, false)
, (18, 'Contract', 'priceInOrder',       true, false)
, (19, 'Contract', 'priceInOrder',       true, true)
, (20, 'Contract', 'priceInOrder',       true, true)
;


insert into "SubProgramContractPermission"
  (contractfield, showtable, showform, parent)
  select 'registrationReason', false, true, parent
  from "SubProgramContractPermission"
  group by parent;

insert into "SubProgramContractPermission"
  (contractfield, showtable, showform, parent)
  select 'priceInOrder', false, true, parent
  from "SubProgramContractPermission"
  group by parent;
