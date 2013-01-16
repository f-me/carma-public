update partner_servicetbl ps
set parentid = ss.pid
from (
select p.id as partner_id, p.sid, s.id as serviceid, s.parentid,
       'partner:' || p.id as pid, p.services
       from
        (select *,
           cast(
                split_part(
                        unnest(
                                string_to_array(
                                        services,
                                        ',')
                                ),
                        ':',
                        2)
               as integer) as sid
               from partnertbl
               where services is not null and services != '') p
       inner join partner_servicetbl s
       on s.id = p.sid
       and s.parentid is null or s.parentid = ''
) ss
where ps.id = ss.serviceid
