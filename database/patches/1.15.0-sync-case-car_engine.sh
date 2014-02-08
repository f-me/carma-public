#!/bin/bash -e

REDIS=$(mktemp /tmp/redisXXXXXX)

TABLE="casetbl"
MODEL="case"
FIELD="car_engine"
FIELDSIZE=${#FIELD}

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', octet_length(coalesce(${FIELD}::text, '')), E'\r\n', ${FIELD}::text, E'\r') from ${TABLE};" > ${REDIS}

cat ${REDIS} | redis-cli --pipe
rm ${REDIS}
