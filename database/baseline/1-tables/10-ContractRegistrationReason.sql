
create table "ContractRegistrationReason"
  ( id serial primary key
  , label text not null default ''
  );

insert into "ContractRegistrationReason" (id, label) values
  (1, 'ТО'),
  (2, 'Ремонт свыше 10000р.');

insert into "FieldPermission" (role, model, field, r, w) values
  (6, 'ContractRegistrationReason', 'id',    true, true)
, (6, 'ContractRegistrationReason', 'label', true, true)
, (7, 'ContractRegistrationReason', 'id',    true, false)
, (7, 'ContractRegistrationReason', 'label', true, false)
;

insert into "Dictionary" (name, description, majorfields) values
  ( 'ContractRegistrationReason'
  , 'Основание для регистрации в программе'
  , '{"label"}'
  );

grant all on "ContractRegistrationReason" to carma_db_sync;
grant all on "ContractRegistrationReason_id_seq" to carma_db_sync;
