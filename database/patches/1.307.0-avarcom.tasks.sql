alter table averagecommissionertbl
  add column tasks
    json not null default '[]';

insert into "FieldPermission" (role, model, field, r, w) values
(1, 'AverageCommissioner', 'tasks', 't', 't');

update "ConstructorFieldOption" set ord = (ord+1) * 2;

insert into "ConstructorFieldOption" (model, program, ord, field, label, r, w)
  select
      model, program, ord+1,
      'tasks', 'Задачи', 't', 't'
    from "ConstructorFieldOption"
    where field = 'times_expectedServiceStart'
      and model = 2; -- AverageCommissioner
