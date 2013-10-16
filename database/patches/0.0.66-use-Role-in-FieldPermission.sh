#!/bin/bash -e

$PSQL -c 'drop table "Role"'
$PSQL -f baseline/3-dictionaries/7-Role.sql

$PSQL <<EOF
  update "FieldPermission"
    set role = r.id
    from "Role" r
    where r.value = role;
EOF

$PSQL <<EOF
  alter table "FieldPermission"
    alter column role type int4 using (role::int4);
  alter table "FieldPermission"
    add foreign key(role) references "Role";
EOF
