
alter table "Program" add column shortLabel text check (trim(shortLabel) <> '');
alter table "Program" add column logo text check (trim(logo) <> '');

insert into "FieldPermission"
  (role, model,     field,        r,    w) values
  (6,    'Program', 'shortLabel', true, true)
 ,(6,    'Program', 'logo',       true, true);
