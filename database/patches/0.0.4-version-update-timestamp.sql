
update version set updated_at = now() where updated_at is null;
alter table version alter updated_at set not null;
alter table version alter updated_at set default now();
