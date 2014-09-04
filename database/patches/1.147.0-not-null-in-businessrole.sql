update "BusinessRole" set roles = '{}'::int[] where roles is null;
alter table "BusinessRole" alter roles set not null;
alter table "BusinessRole" alter roles set default '{}'::int[];
