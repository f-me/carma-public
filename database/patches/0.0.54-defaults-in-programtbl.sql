
update programtbl set active = false where active is null;
alter table programtbl alter column active set default false;
alter table programtbl alter column active set not null;

update programtbl set value = '' where value is null;
alter table programtbl alter column value set default '';
alter table programtbl alter column value set not null;

delete from programtbl where label is null;
alter table programtbl alter column label set default '';
alter table programtbl alter column label set not null;

update programtbl set client = '' where client is null;
alter table programtbl alter column client set default '';
alter table programtbl alter column client set not null;

update programtbl set clientcode = '' where clientcode is null;
alter table programtbl alter column clientcode set default '';
alter table programtbl alter column clientcode set not null;

update programtbl set clientaddress = '' where clientaddress is null;
alter table programtbl alter column clientaddress set default '';
alter table programtbl alter column clientaddress set not null;

update programtbl set services = array[]::text[] where services is null;
alter table programtbl alter column services set default array[]::text[];
alter table programtbl alter column services set not null;

update programtbl set contracts = '' where contracts is null;
alter table programtbl alter column contracts set default '';
alter table programtbl alter column contracts set not null;

update programtbl set help = '' where help is null;
alter table programtbl alter column help set default '';
alter table programtbl alter column help set not null;
