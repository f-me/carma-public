create sequence partnertbl_id_seq;
alter table partnertbl alter column id set default nextval('partnertbl_id_seq');
select setval('partnertbl_id_seq', max(id)) from partnertbl;
grant all on "partnertbl_id_seq" to carma_db_sync;
grant all on "partnertbl_id_seq" to carma_search;
