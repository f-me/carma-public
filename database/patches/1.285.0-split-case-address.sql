
alter table casetbl add column caseaddress_city int4 references "City";

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select model, program, ord+1, 'caseAddress_city', 'Город', r, w
    from "ConstructorFieldOption"
    where field = 'caseAddress_address' and model = 1;

