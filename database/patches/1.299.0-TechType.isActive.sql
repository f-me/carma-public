
alter table "TechType" add column isActive bool not null default 't';

insert into "FieldPermission" (role, model, field, r, w) values
 (6, 'TechType', 'isActive', 't', 't')
,(7, 'TechType', 'isActive', 't', 'f')
;

