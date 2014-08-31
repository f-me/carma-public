
-- add primary key sequence
create sequence usermetatbl_id_seq;
alter table usermetatbl alter column id set default nextval('usermetatbl_id_seq');
select setval('usermetatbl_id_seq', max(id)) from usermetatbl;
grant all on "usermetatbl_id_seq" to carma_db_sync;
grant all on "usermetatbl_id_seq" to carma_search;


delete from usermetatbl where uid is null;
alter table usermetatbl alter uid set not null;

update usermetatbl set isactive = false where isactive is null;
alter table usermetatbl alter isactive set not null;
alter table usermetatbl alter isactive set default false;

update usermetatbl
  set isactive = false, realName = id :: text
  where realName is null or realName ~ '^\s*$';
alter table usermetatbl alter realName set not null;
alter table usermetatbl add constraint realName_is_valid check (realName !~ '^\s*$');

alter table usermetatbl alter login set not null;
alter table usermetatbl add constraint login_is_uniqie unique (login);
alter table usermetatbl add constraint login_is_valid check (login !~ '^\s*$');

update usermetatbl set roles = '{}'::text[] where roles is null;
alter table usermetatbl alter roles set not null;
alter table usermetatbl alter roles set default '{}'::int[];

update usermetatbl set programs = '{}'::text[] where programs is null;
alter table usermetatbl alter programs set not null;
alter table usermetatbl alter programs set default '{}'::int[];

update usermetatbl set bocities = '{}'::text[] where bocities is null;
alter table usermetatbl alter bocities set not null;
alter table usermetatbl alter bocities set default '{}'::int[];

update usermetatbl set boprograms = '{}'::text[] where boprograms is null;
alter table usermetatbl alter boprograms set not null;
alter table usermetatbl alter boprograms set default '{}'::int[];

update usermetatbl set isdealer = false where isdealer is null;
alter table usermetatbl alter isdealer set not null;
alter table usermetatbl alter isdealer set default false;

update usermetatbl set workPhone = '' where workPhone is null;
alter table usermetatbl alter workPhone set not null;
alter table usermetatbl alter workPhone set default '';

update usermetatbl set workPhonesuffix = '' where workPhonesuffix is null;
alter table usermetatbl alter workPhonesuffix set not null;
alter table usermetatbl alter workPhonesuffix set default '';

update usermetatbl set mobilePhone = '' where mobilePhone is null;
alter table usermetatbl alter mobilePhone set not null;
alter table usermetatbl alter mobilePhone set default '';

update usermetatbl set homePhone = '' where homePhone is null;
alter table usermetatbl alter homePhone set not null;
alter table usermetatbl alter homePhone set default '';

update usermetatbl set email = '' where email is null;
alter table usermetatbl alter email set not null;
alter table usermetatbl alter email set default '';

update usermetatbl set position = '' where position is null;
alter table usermetatbl alter position set not null;
alter table usermetatbl alter position set default '';

update usermetatbl set lastactivity = '2012-01-01' where lastactivity is null;
alter table usermetatbl alter lastactivity set not null;
alter table usermetatbl alter lastactivity set default now();

update usermetatbl set lastlogout = '2012-01-01' where lastlogout is null;
alter table usermetatbl alter lastlogout set not null;
alter table usermetatbl alter lastlogout set default now();
