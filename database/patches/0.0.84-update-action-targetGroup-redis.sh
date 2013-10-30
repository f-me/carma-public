#!/bin/bash -e

REDIS=$(mktemp /tmp/redisXXXXXX)

MODEL="action"
FIELD="targetGroup"
FIELDSIZE=${#FIELD}

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', length(coalesce(${FIELD}, '')), E'\r\n', ${FIELD}, E'\r') from actiontbl;" > ${REDIS}

cat ${REDIS} | redis-cli --pipe
rm ${REDIS}
