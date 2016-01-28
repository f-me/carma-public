insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 17, p.id, 847, 'check1', 'Заблокирован электронный ручной тормоз', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 17, p.id, 847, 'check2', 'Руль заблокирован', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check1', 'Капот открывается', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check2', 'Наличие запасного колеса', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check3', 'Наличие секреток', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check4', 'Запасной ключ имеется', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check5', 'Документы на автомобиль на руках', 't', 't'
    from "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select 14, p.id, 674, 'check6', 'Не открывается лючок бензобака', 't', 't'
    from "Program" p;


insert into "FieldPermission" (role, model, field, r, w) values (1, 'Towage', 'check1', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Towage', 'check2', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check1', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check2', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check3', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check4', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check5', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check', 't', 't');
insert into "FieldPermission" (role, model, field, r, w) values (1, 'Tech', 'check6', 't', 't');

alter table towagetbl add column check1 boolean;
alter table towagetbl add column check2 boolean;
alter table techtbl add column check1 boolean;
alter table techtbl add column check2 boolean;
alter table techtbl add column check3 boolean;
alter table techtbl add column check4 boolean;
alter table techtbl add column check5 boolean;
alter table techtbl add column check6 boolean;

