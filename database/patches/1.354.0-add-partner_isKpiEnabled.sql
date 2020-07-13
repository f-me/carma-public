alter table partnertbl add column isKpiEnabled bool not null default false;

insert into "FieldPermission" (role, model, field, r, w) values
  (1, 'Partner', 'isKpiEnabled', true, false)
, (3, 'Partner', 'isKpiEnabled', true, true);
