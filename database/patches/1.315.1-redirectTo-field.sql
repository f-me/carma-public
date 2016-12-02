

alter table calltbl add redirectTo int references usermetatbl(id);

insert into "FieldPermission"
  (role, model, field, r, w) values
  (1, 'Call', 'redirectTo', true, true);
