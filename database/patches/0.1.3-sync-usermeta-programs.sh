#!/bin/bash -e

REDIS=$(mktemp /tmp/redisXXXXXX)

TABLE="usermetatbl"
MODEL="usermeta"
FIELD="programs"
FIELDSIZE=${#FIELD}

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', length(coalesce(array_to_string(${FIELD},','), '')), E'\r\n', array_to_string(${FIELD},','), E'\r') from ${TABLE};" > ${REDIS}

cat ${REDIS} | redis-cli --pipe
rm ${REDIS}
