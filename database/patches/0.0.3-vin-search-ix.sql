
create index on contracttbl using gist(lower(carVin) gist_trgm_ops);
