
$PSQL -c 'create index on "Event" (ctime, modelid)'
$PSQL -f baseline/5-views/12-case-history.sql
