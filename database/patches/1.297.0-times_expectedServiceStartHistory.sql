

alter table servicetbl
  add column times_expectedServiceStartHistory
    json not null default '[]';

insert into "FieldPermission" (role, model, field, r, w)
  select
      1, model, 'times_expectedServiceStartHistory', 't', 't'
    from "FieldPermission"
    where field = 'times_expectedServiceStart';


update "ConstructorFieldOption" set ord = (ord+1) * 2;

insert into "ConstructorFieldOption" (model, program, ord, field, label, r, w)
  select
      model, program, ord+1,
      'times_expectedServiceStartHistory',
      'История ОВНОУ',
      't', 't'
    from "ConstructorFieldOption"
    where field = 'times_expectedServiceStart';
