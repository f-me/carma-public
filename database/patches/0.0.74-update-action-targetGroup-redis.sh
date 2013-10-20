#!/bin/bash -e

SQL=$(mktemp /tmp/sqlXXXXXX)
REDIS=$(mktemp /tmp/redisXXXXXX)

psql carma -A -t -F ';' -c "select id,targetGroup from actiontbl" > ${SQL}

MODEL="action"
FIELD="targetGroup"
FIELDSIZE=${#FIELD}

function add_hset ()
{
    key="$MODEL:$1"
    keysize=${#key}
    value=${2}
    valuesize=${#value}
    echo -e -n "*4\r\n\$4\r\nHSET\r\n\$${keysize}\r\n${key}\r\n\$${FIELDSIZE}\r\n${FIELD}\r\n\$${valuesize}\r\n${value}\r\n" >> ${REDIS}
}

while read line
do
    key=$(echo ${line} | cut -d';' -f1)
    value=$(echo ${line} | cut -d';' -f2)
    add_hset "$key" "$value"
done < ${SQL}

cat ${REDIS} | redis-cli --pipe
rm ${SQL} ${REDIS}
