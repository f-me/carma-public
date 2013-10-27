
create extension pg_trgm;
create index on casetbl using gist(lower(car_vin) gist_trgm_ops);
create index on casetbl using gist(lower(car_plateNum) gist_trgm_ops);
create index on casetbl using gist(lower(contact_phone1) gist_trgm_ops) where contact_phone1 is not null;
create index on casetbl using gist(lower(contact_phone2) gist_trgm_ops) where contact_phone2 is not null;
create index on casetbl using gist(lower(contact_phone3) gist_trgm_ops) where contact_phone3 is not null;
create index on casetbl using gist(lower(contact_phone4) gist_trgm_ops) where contact_phone4 is not null;
create index on casetbl using gist(lower(contact_ownerPhone1) gist_trgm_ops) where contact_ownerPhone1 is not null;
create index on casetbl using gist(lower(contact_ownerPhone2) gist_trgm_ops) where contact_ownerPhone2 is not null;
create index on casetbl using gist(lower(contact_ownerPhone3) gist_trgm_ops) where contact_ownerPhone3 is not null;
