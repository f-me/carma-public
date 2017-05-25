

alter table "DiagSlide" add column isActive bool not null default true;
insert into "FieldPermission" (role, model, field, r, w) values
  (1, 'DiagSlide', 'isActive', true, true)
;
