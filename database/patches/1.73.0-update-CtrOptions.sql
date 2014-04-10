
update "ConstructorFieldOption"
  set r = 't', w = 't'
  where field = 'contract';

update "ConstructorFieldOption"
  set r = 'f', w = 'f'
  where field = 'createTime' and screen = 1;

update "ConstructorFieldOption"
  set r = 't', w = 'f'
  where field = 'createTime' and screen = 2;


delete from "ConstructorFieldOption"
  where program = 0;

drop table if exists "NewCaseField";
