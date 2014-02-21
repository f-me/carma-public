update partnertbl set emails = '[]' :: json
  where emails is null or emails::text = 'null';
alter table partnertbl alter column emails set default '[]' :: json;
alter table partnertbl alter column emails set not null;
