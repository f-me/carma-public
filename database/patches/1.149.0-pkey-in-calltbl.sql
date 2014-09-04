delete from calltbl
  where ctid in
    (select max(ctid) from calltbl group by id having count(*) > 1);
alter table calltbl add constraint calltbl_pkey primary key (id);
