alter table "ProcessingConfig" add column avgSuburbanSpeed numeric(5,2) not null default 60;


insert into "FieldPermission" (role, model, field, r, w)
  values
   (13, 'ProcessingConfig', 'avgSuburbanSpeed', 't', 't'),
   (14, 'ProcessingConfig', 'avgSuburbanSpeed', 't', 'f'),
   ( 2, 'ProcessingConfig', 'avgSuburbanSpeed', 't', 'f');
