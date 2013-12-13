#!/bin/bash -e

REDIS=$(mktemp /tmp/redisXXXXXX)

TABLE="usermetatbl"
MODEL="usermeta"
FIELD="programs"
FIELDSIZE=${#FIELD}

$PSQL -c 'drop table "SubProgram"'
$PSQL -c 'drop table "Program"'
$PSQL -f baseline/3-dictionaries/22-Program.sql
$PSQL -f baseline/3-dictionaries/23-SubProgram.sql

${PSQL} -f patches/0.1.2-usermeta-programs.sql

${PSQL} -A -t -c "select concat(E'*4\r\n', '$', E'4\r\nHSET\r\n', '$', length(concat('${MODEL}:', id)), E'\r\n', concat('${MODEL}:', id), E'\r\n', '$', ${FIELDSIZE}, E'\r\n${FIELD}\r\n', '$', octet_length(coalesce(array_to_string(${FIELD},','), '')), E'\r\n', array_to_string(${FIELD},','), E'\r') from ${TABLE};" > ${REDIS}

cat ${REDIS} | redis-cli --pipe
rm ${REDIS}
