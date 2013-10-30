with svc as
  (select cid, split_part(f,':',1) as t, split_part(f,':',2) as i
    from (select id as cid, regexp_split_to_table(services, ',') as f
      from casetbl where coalesce(services,'') <> '') x)
  update servicetbl set parentid = ('case:' || y.cid)
    from (select svc.cid, svc.t, svc.i
      from svc, servicetbl s
      where s.type = svc.t
        and s.id = svc.i::int
        and coalesce(s.parentid,'case:null') = 'case:null') y
    where servicetbl.id = y.i::int
      and servicetbl.type = y.t;

delete from servicetbl where parentid is null or parentid = 'case:null';

alter table servicetbl alter column parentid set not null;
