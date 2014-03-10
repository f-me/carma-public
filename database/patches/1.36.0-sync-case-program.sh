#!/bin/bash -e

REDIS=$(mktemp /tmp/redisXXXXXX)

TABLE="casetbl"
MODEL="case"
FIELD="program"
FIELDSIZE=${#FIELD}

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', octet_length(coalesce(${FIELD}::text, '')), E'\r\n', ${FIELD}, E'\r') from ${TABLE};" > ${REDIS}

cat ${REDIS} | redis-cli --pipe

TABLE="casetbl"
MODEL="case"
FIELD="subprogram"
FIELDSIZE=${#FIELD}

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', octet_length(coalesce(${FIELD}::text, '')), E'\r\n', ${FIELD}::text, E'\r') from ${TABLE};" > ${REDIS}

cat ${REDIS} | redis-cli --pipe

rm ${REDIS}

# servicesview dropped in 1.34.*-case-call-program-fks.sql:

$PSQL -f baseline/5-views/2-services-view.sql
