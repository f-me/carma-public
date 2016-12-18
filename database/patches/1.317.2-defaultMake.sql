

alter table "SubProgram" add column defaultMake int references "CarModel"(id);

insert into "FieldPermission" (role, model, field, r, w) values
  (6, 'SubProgram', 'defaultMake', true, true)
, (7, 'SubProgram', 'defaultMake', true, false)
;
