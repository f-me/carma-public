#!/usr/bin/env bash
initdb -D /pgdata                              || exit $?
{ postgres -D /pgdata -h localhost -p 5432 & } || exit $?
PGPID=$!
sleep 2

echo $'
	CREATE USER carma         WITH SUPERUSER PASSWORD \'pass\';
	CREATE USER carma_db_sync WITH SUPERUSER PASSWORD \'pass\';
	ALTER  USER postgres      WITH           PASSWORD \'pass\';
' | psql -h localhost || exit $?

echo 'CREATE DATABASE carma;' \
	| psql -h localhost -U carma_db_sync -d postgres || exit $?

kill -TERM "$PGPID" || exit $?
sleep 2
kill -KILL "$PGPID" 2>/dev/null
sleep 1
chmod go-rwx -R /pgdata || exit $?
