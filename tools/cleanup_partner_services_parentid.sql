update partner_servicetbl
set parentId = null
where parentId is not null AND parentId != ''
AND id not in (select cast(split_part(p.r, ':', 2) as integer) from
               (select regexp_split_to_table(services, ',') r from partnertbl) p
               where p.r != '');
