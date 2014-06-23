update calltbl
  set carMake = "CarMake".id
  from calltbl c left join "CarMake"
    on carMake = "CarMake".value :: text
  where calltbl.id = c.id
    and calltbl.carMake is not null;

update calltbl
  set carModel = "CarModel".id
  from calltbl c left join "CarModel"
    on (carModel = "CarModel".value :: text and carMake = "CarModel".parent :: text)
  where calltbl.id = c.id
    and calltbl.carModel is not null;

alter table calltbl alter carMake type int using carMake::int;
alter table calltbl add constraint calltbl_carMake_fkey foreign key (carMake) references "CarMake";
alter table calltbl alter carModel type int using carModel::int;
alter table calltbl add constraint calltbl_carModel_fkey foreign key (carModel) references "CarModel";

update casetbl
  set car_make = "CarMake".id
  from casetbl c left join "CarMake"
    on car_make = "CarMake".value :: text
  where casetbl.id = c.id
    and casetbl.car_make is not null;

update casetbl
  set car_model = "CarModel".id
  from casetbl c left join "CarModel"
    on (car_model = "CarModel".value :: text and car_make = "CarModel".parent :: text)
  where casetbl.id = c.id
    and casetbl.car_model is not null;

alter table casetbl alter car_make type int using car_make::int;
alter table casetbl add constraint casetbl_car_make_fkey foreign key (car_make) references "CarMake";
alter table casetbl alter car_model type int using car_model::int;
alter table casetbl add constraint casetbl_car_model_fkey foreign key (car_model) references "CarModel";

update renttbl
  set rentedMake = "CarMake".id
  from renttbl c left join "CarMake"
    on rentedMake = "CarMake".value :: text
  where renttbl.id = c.id
    and renttbl.rentedMake is not null;

update renttbl
  set rentedModel = "CarModel".id
  from renttbl c left join "CarModel"
    on (rentedModel = "CarModel".value :: text and rentedMake = "CarModel".parent :: text)
  where renttbl.id = c.id
    and renttbl.rentedModel is not null;

alter table renttbl alter rentedMake type int using rentedMake::int;
alter table renttbl add constraint renttbl_car_rentedMake_fkey foreign key (rentedMake) references "CarMake";
alter table renttbl alter rentedModel type int using rentedModel::int;
alter table renttbl add constraint renttbl_car_rentedModel_fkey foreign key (rentedModel) references "CarModel";


update partnertbl
  set makes = x.new_makes
  from (
    select p.id, array_agg("CarMake".id) as new_makes
      from partnertbl p, unnest(p.makes) m, "CarMake"
      where m = "CarMake".value
      group by p.id) x
  where partnertbl.id = x.id;

update partnertbl set makes = '{}'::int[] where makes is null;
alter table partnertbl alter makes type int[] using makes::int[];
alter table partnertbl alter makes set not null;
alter table partnertbl alter makes set default '{}'::int[];

