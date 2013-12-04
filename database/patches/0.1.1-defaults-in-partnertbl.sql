update partnertbl set addrs = '[]'::json where coalesce(addrs::text,'null') = 'null';
update partnertbl set phones = '[]'::json where coalesce(phones::text,'null') = 'null';
alter table partnertbl alter COLUMN addrs set not null;
alter table partnertbl alter COLUMN phones set not null;
alter table partnertbl alter COLUMN phones set default '[]'::json;
alter table partnertbl alter COLUMN addrs set default '[]'::json;

