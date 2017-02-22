
alter table "SubProgram" add column diagTree int references "DiagSlide"(id);

insert into "FieldPermission"
  (role, model, field, r, w) values
  (1, 'SubProgram', 'diagTree', true, true);
