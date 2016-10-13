with  calls_with_neighbors as
    (select id,
        (select max(x.id) from calltbl x where x.id < c.id and x.calldate > '2000-01-01') as prev_call_with_date,
        (select min(x.id) from calltbl x where x.id > c.id and x.calldate > '2000-01-01') as next_call_with_date
      from calltbl c
      where calldate < '2000-01-01'),
  calls_with_date as
    (select
        c.id as cid, p.id as pid, n.id as nid, tstzrange(p.calldate, n.calldate) as range
      from calls_with_neighbors c
        right join calltbl p on (p.id = c.prev_call_with_date)
        join calltbl n on (n.id = c.next_call_with_date))
  update calltbl c
    set calldate = upper(d.range)
    from calls_with_date d
    where d.cid = c.id;
