#!/bin/bash -e

sql=$(cat <<EOF
with
  rels as (SELECT c.relname AS child, p.relname AS parent
             FROM pg_inherits JOIN pg_class AS c ON (inhrelid=c.oid)
             JOIN pg_class as p ON (inhparent=p.oid))

select
  t.table_name,
  string_agg(c.column_name :: text, ';')
from information_schema.tables as t
inner join information_schema.columns as c
on c.table_name = t.table_name
left join rels as r
on t.table_name = r.child

where t.table_schema = 'public'
and t.table_type = 'BASE TABLE'
and c.data_type = 'timestamp without time zone'
and (r.parent is null or
     (c.column_name not in
       (select column_name
        from information_schema.columns
        where table_name = r.parent)))

group by t.table_name
EOF
)

commands=""

for i in `$PSQL -tA -c "$sql" `; do
    IFS='|' read -a arr <<< "$i"
    IFS=';' read -a cs  <<< "${arr[1]}"
    alts="ALTER COLUMN ${cs[0]} TYPE timestamptz"
    for c in ${cs[@]:1}; do
        alts="$alts, ALTER COLUMN $c TYPE timestamptz"
    done
    c="ALTER TABLE ${arr[0]} $alts"
    commands="$commands $c;"
done

$PSQL -c "DROP VIEW servicesview"
$PSQL -c "DROP VIEW partnercancelview"
$PSQL -c "$commands"
$PSQL -f baseline/5-views/2-services-view.sql
$PSQL -c "GRANT SELECT ON servicesview TO carma_db_sync"
$PSQL -f baseline/5-views/1-partnercancel-view.sql
