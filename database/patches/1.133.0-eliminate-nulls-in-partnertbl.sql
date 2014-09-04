
alter table partnertbl drop column garbage;
alter table partnertbl drop column makers;

update partnertbl set isactive = false where isactive is null;
alter table partnertbl alter isactive set not null;
alter table partnertbl alter isactive set default false;

update partnertbl set isdealer = false where isdealer is null;
alter table partnertbl alter isdealer set not null;
alter table partnertbl alter isdealer set default false;

update partnertbl set ismobile = false where ismobile is null;
alter table partnertbl alter ismobile set not null;
alter table partnertbl alter ismobile set default false;

update partnertbl set ispaybackconfirmed = false where ispaybackconfirmed is null;
alter table partnertbl alter ispaybackconfirmed set not null;
alter table partnertbl alter ispaybackconfirmed set default false;

update partnertbl set isfree = false where isfree is null;
alter table partnertbl alter isfree set not null;
alter table partnertbl alter isfree set default false;

update partnertbl
  set isactive = false, name = id :: text
  where name is null or name ~ '^\s*$';
alter table partnertbl alter name set not null;
alter table partnertbl add constraint name_is_valid check (name !~ '^\s*$');

update partnertbl set services = '' where services is null;
alter table partnertbl alter services set not null;
alter table partnertbl alter services set default '';

update partnertbl set comment = '' where comment is null;
alter table partnertbl alter comment set not null;
alter table partnertbl alter comment set default '';

update partnertbl set synonyms = '{}'::text[] where synonyms is null;
alter table partnertbl alter synonyms set not null;
alter table partnertbl alter synonyms set default '{}'::int[];

update partnertbl set mtime = now() where mtime is null;
alter table partnertbl alter mtime set not null;
alter table partnertbl alter mtime set default now();
