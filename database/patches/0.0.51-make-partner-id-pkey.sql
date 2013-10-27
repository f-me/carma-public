delete from partnertbl
  where ctid in
    (select max(ctid) from partnertbl
      where id in
        (select id from partnertbl group by id having count(*) > 1)
      group by id);
ALTER TABLE partnertbl ADD PRIMARY KEY (id);
