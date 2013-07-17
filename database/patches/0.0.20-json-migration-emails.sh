#!/bin/bash -e

export CARMA_PORT=${1:-8000}

OUTPUT=$(mktemp /tmp/emailsXXXXXX-tmp)
OUTPUT2=$(mktemp /tmp/emailsXXXXXX-out)

curl -s "http://localhost:${CARMA_PORT}/all/partner" | \
    jq -c -r '.[] | .id + "|\(if .closeTicketEmail and ((.closeTicketEmail) == "" | not) then [{key:("close"), value:.closeTicketEmail}] else [] end)"' > ${OUTPUT}

while read line
do
    pid=$(echo ${line} | cut -d'|' -f1 | cut -d: -f2)
    req=$(echo ${line} | cut -d'|' -f2)
    echo curl -X PUT localhost:${CARMA_PORT}/_/partner/${pid} --data "'"'{"emails":"'$(echo ${req} | sed -e 's/\"/\\\"/g')'"}'"'" | grep -v "\[\]" >> ${OUTPUT2}
done < ${OUTPUT}

less ${OUTPUT2}
bash ${OUTPUT2}
rm ${OUTPUT}
rm ${OUTPUT2}
