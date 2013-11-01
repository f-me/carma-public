#!/bin/bash -e

sql=$(cat <<EOF
with
  rels as (SELECT c.relname AS child, p.relname AS parent
             FROM pg_inherits JOIN pg_class AS c ON (inhrelid=c.oid)
             JOIN pg_class as p ON (inhparent=p.oid))

select
  t.table_name,
  c.column_name
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
        where table_name = r.parent)));

EOF
)

commands=""

for i in `$PSQL -tA -c "$sql" `; do
    IFS='|' read -a arr <<< "$i"
    c="ALTER TABLE ${arr[0]} ALTER COLUMN ${arr[1]} TYPE timestamptz"
    commands="$commands $c;"
done
$PSQL -c "DROP VIEW servicesview"
$PSQL -c "DROP VIEW partnercancelview"
$PSQL -c "$commands"
$PSQL -f baseline/5-views/0-services-view.sql
$PSQL -c "GRANT SELECT ON servicesview TO carma_db_sync"
$PSQL -f baseline/5-views/1-partnercancel-view.sql
