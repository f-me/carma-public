#!/usr/bin/env bash

IP=`hostname -i` || exit $?

echo "
local   all   all                  trust
host    all   all   127.0.0.1/32   trust
host    all   all   $IP/24         md5
" > /pgdata/pg_hba.conf || exit $?

chmod go-rwx -R /pgdata                || exit $?
postgres -D /pgdata -h 0.0.0.0 -p 5432 || exit $?
