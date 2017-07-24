#!/usr/bin/env bash
set -e

IP=`hostname -i`

echo "
local   all   all                  trust
host    all   all   127.0.0.1/32   trust
host    all   all   $IP/24         md5
" > "$PGDATA/pg_hba.conf"

perl -p -i -e 's/^unix_socket_directories = .*\n//' "$PGDATA/postgresql.conf"
echo $'unix_socket_directories = \'/tmp\'' >> "$PGDATA/postgresql.conf"
chmod go-rwx -R "$PGDATA"
postgres -D "$PGDATA" -h 0.0.0.0 -p 5432
