-- add primary key sequence
create sequence calltbl_id_seq;
alter table calltbl alter column id set default nextval('calltbl_id_seq');
select setval('calltbl_id_seq', max(id)) from calltbl;
grant all on "calltbl_id_seq" to carma_db_sync;
grant all on "calltbl_id_seq" to carma_search;
