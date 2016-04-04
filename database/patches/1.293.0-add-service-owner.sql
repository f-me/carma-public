
alter table servicetbl add owner int references usermetatbl;
update servicetbl set owner = creator;
alter table servicetbl alter column owner set not null;
