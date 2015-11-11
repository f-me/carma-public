
begin;

insert into "Role" (id, value, label, isback, hidden) values
 (61, 'consultant_mech', 'Консультант: Механик', false, false)
,(62, 'consultant_tech', 'Консультант: Теххелпер', false, false);

update "Role"
  set value = 'consultant_op', label = 'Консультант: Оператор'
  where value = 'consultant';

update "ConsultationType"
  set label = 'Консультация механика и теххелпера'
  where id = 2;

commit;
