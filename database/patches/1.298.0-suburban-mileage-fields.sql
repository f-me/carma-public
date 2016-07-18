
begin;

drop view allservicesview cascade;

update servicetbl
  set suburbanMilage =
    case when suburbanMilage ~ '^\s*\d+([\.,]\d+)?\s*$'
      then replace(suburbanMilage, ',', '.')
      else '0'
    end;

alter table servicetbl alter column suburbanMilage
  type numeric(7,2) using suburbanMilage::numeric(7,2);

alter table renttbl add column totalMilage numeric(7,2);
alter table renttbl add column isCountryRide boolean not null default 'f';
alter table renttbl add column partnerWarnedInTime boolean;

alter table techtbl add column totalMilage numeric(7,2);
alter table techtbl add column isCountryRide boolean not null default 'f';
alter table techtbl add column partnerWarnedInTime boolean;

alter table towagetbl add column totalMilage numeric(7,2);
alter table towagetbl add column isCountryRide boolean not null default 'f';
alter table towagetbl add column partnerWarnedInTime boolean;

alter table taxitbl add column totalMilage numeric(7,2);
alter table taxitbl add column isCountryRide boolean not null default 'f';
alter table taxitbl add column partnerWarnedInTime boolean;

alter table sobertbl add column totalMilage numeric(7,2);
alter table sobertbl add column isCountryRide boolean not null default 'f';
alter table sobertbl add column partnerWarnedInTime boolean;


update averagecommissionertbl
  set suburbanMilage =
    case when commMilage ~ '^\s*\d+([\.,]\d+)?\s*$'
      then replace(commMilage, ',', '.')::numeric(7,2)
      else 0
    end;
alter table averagecommissionertbl drop column commMilage;
alter table averagecommissionertbl add column totalMilage numeric(7,2);
alter table averagecommissionertbl add column isCountryRide boolean not null default 'f';
alter table averagecommissionertbl add column partnerWarnedInTime boolean;


delete from "FieldPermission" where field = 'suburbanMilage' or field = 'commMilage';
insert into "FieldPermission" (role, model, field, r, w)
  values
   ( 1, 'Rent', 'suburbanMilage', 't', 'f'),
   (14, 'Rent', 'suburbanMilage', 't', 't'),
   ( 1, 'Rent', 'totalMilage',    't', 'f'),
   (14, 'Rent', 'totalMilage',    't', 't'),
   ( 1, 'Rent', 'isCountryRide',  't', 'f'),
   (14, 'Rent', 'isCountryRide',  't', 't'),
   ( 1, 'Rent', 'partnerWarnedInTime',  't', 'f'),
   (14, 'Rent', 'partnerWarnedInTime',  't', 't'),

   ( 1, 'Tech', 'suburbanMilage', 't', 'f'),
   (14, 'Tech', 'suburbanMilage', 't', 't'),
   ( 1, 'Tech', 'totalMilage',    't', 'f'),
   (14, 'Tech', 'totalMilage',    't', 't'),
   ( 1, 'Tech', 'isCountryRide',  't', 'f'),
   (14, 'Tech', 'isCountryRide',  't', 't'),
   ( 1, 'Tech', 'partnerWarnedInTime',  't', 'f'),
   (14, 'Tech', 'partnerWarnedInTime',  't', 't'),

   ( 1, 'Towage', 'suburbanMilage', 't', 'f'),
   (14, 'Towage', 'suburbanMilage', 't', 't'),
   ( 1, 'Towage', 'totalMilage',    't', 'f'),
   (14, 'Towage', 'totalMilage',    't', 't'),
   ( 1, 'Towage', 'isCountryRide',  't', 'f'),
   (14, 'Towage', 'isCountryRide',  't', 't'),
   ( 1, 'Towage', 'partnerWarnedInTime',  't', 'f'),
   (14, 'Towage', 'partnerWarnedInTime',  't', 't'),

   ( 1, 'Taxi', 'suburbanMilage', 't', 'f'),
   (14, 'Taxi', 'suburbanMilage', 't', 't'),
   ( 1, 'Taxi', 'totalMilage',    't', 'f'),
   (14, 'Taxi', 'totalMilage',    't', 't'),
   ( 1, 'Taxi', 'isCountryRide',  't', 'f'),
   (14, 'Taxi', 'isCountryRide',  't', 't'),
   ( 1, 'Taxi', 'partnerWarnedInTime',  't', 'f'),
   (14, 'Taxi', 'partnerWarnedInTime',  't', 't'),

   ( 1, 'SoberDriver', 'suburbanMilage', 't', 'f'),
   (14, 'SoberDriver', 'suburbanMilage', 't', 't'),
   ( 1, 'SoberDriver', 'totalMilage',    't', 'f'),
   (14, 'SoberDriver', 'totalMilage',    't', 't'),
   ( 1, 'SoberDriver', 'isCountryRide',  't', 'f'),
   (14, 'SoberDriver', 'isCountryRide',  't', 't'),
   ( 1, 'SoberDriver', 'partnerWarnedInTime',  't', 'f'),
   (14, 'SoberDriver', 'partnerWarnedInTime',  't', 't'),

   ( 1, 'AverageCommissioner', 'suburbanMilage', 't', 'f'),
   (14, 'AverageCommissioner', 'suburbanMilage', 't', 't'),
   ( 1, 'AverageCommissioner', 'totalMilage',    't', 'f'),
   (14, 'AverageCommissioner', 'totalMilage',    't', 't'),
   ( 1, 'AverageCommissioner', 'isCountryRide',  't', 'f'),
   (14, 'AverageCommissioner', 'isCountryRide',  't', 't'),
   ( 1, 'AverageCommissioner', 'partnerWarnedInTime',  't', 'f'),
   (14, 'AverageCommissioner', 'partnerWarnedInTime',  't', 't');



delete from "ConstructorFieldOption" where  field = 'suburbanMilage';
delete from "ConstructorFieldOption" where  field = 'commMilage';
update "ConstructorFieldOption" set ord = (ord+1) * 4;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select
      model.id, p.id, coalesce(
        (select ord+1 from "ConstructorFieldOption"
          where model = model.id and program = p.id and field = 'contractor_partner'),
        0),
      'isCountryRide', 'За городом', 't', 't'
    from
      (values(2),(11),(12),(13),(14),(17)) model(id),
      "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select
      model.id, p.id, coalesce(
        (select ord+2 from "ConstructorFieldOption"
          where model = model.id and program = p.id and field = 'contractor_partner'),
        0),
      'suburbanMilage', 'Пробег за городом', 't', 't'
    from
      (values(2),(11),(12),(13),(14),(17)) model(id),
      "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select
      model.id, p.id, coalesce(
        (select ord+3 from "ConstructorFieldOption"
          where model = model.id and program = p.id and field = 'contractor_partner'),
        0),
      'totalMilage', 'Километраж по тахометру', 't', 't'
    from
      (values(2),(11),(12),(13),(14),(17)) model(id),
      "Program" p;

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  select
      model.id, p.id, coalesce(
        (select ord+4 from "ConstructorFieldOption"
          where model = model.id and program = p.id and field = 'contractor_partner'),
        0),
      'partnerWarnedInTime', 'Партнёр предупредил вовремя', 't', 't'
    from
      (values(2),(11),(12),(13),(14),(17)) model(id),
      "Program" p;

commit;
