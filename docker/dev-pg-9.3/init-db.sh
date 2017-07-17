#!/usr/bin/env bash
set -e

initdb -D /pgdata
postgres -D /pgdata -h localhost -p 5432 &
PGPID=$!
sleep 2

echo $'
	CREATE USER carma         WITH SUPERUSER PASSWORD \'pass\';
	CREATE USER carma_db_sync WITH SUPERUSER PASSWORD \'pass\';
	ALTER  USER postgres      WITH           PASSWORD \'pass\';
' | psql -h localhost

echo 'CREATE DATABASE carma;' \
	| psql -h localhost -U carma_db_sync -d postgres

kill -TERM "$PGPID"
sleep 2
kill -KILL "$PGPID" 2>/dev/null || true
sleep 1
chmod go-rwx -R /pgdata
