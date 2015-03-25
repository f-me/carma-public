alter table "Program" add column active boolean not null default true;
insert into "FieldPermission"
  (role, model, field, r ,w) values
  (1, 'Program', 'active', true, false)
, (6, 'Program', 'active', true, true)
, (12,'Program', 'active', true, true)
, (13,'Program', 'active', true, true)
, (35,'Program', 'active', true, true)
;
