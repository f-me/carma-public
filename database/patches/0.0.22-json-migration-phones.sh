#!/bin/bash -e

export CARMA_PORT=${CARMA_PORT:=8000}

OUTPUT=$(mktemp /tmp/phonesXXXXXX-tmp)
OUTPUT2=$(mktemp /tmp/phonesXXXXXX-out)

curl -s "http://localhost:${CARMA_PORT}/all/partner" | \
    jq -c -r '.[] | .id + "|\(if .phone1 and ((.phone1) == "" | not) then [{key:("disp"), value:.phone1, note: (if .workingTime and ((.workingTime) == "" | not) then .workingTime else "" end)}] else [] end + if .salesPhone and ((.salesPhone) == "" | not) then [{key:("sales"), value:.salesPhone, note: (if .salesWorking and ((.salesWorking) == "" | not) then .salesWorking else "" end)}] else [] end + if .servicePhone and ((.servicePhone) == "" | not) then [{key:("serv"), value:.servicePhone, note: (if .serviceWorking and ((.serviceWorking) == "" | not) then .serviceWorking else "" end)}] else [] end + if .closeTicketPhone and ((.closeTicketPhone) == "" | not) then [{key:("close"), value:.closeTicketPhone}] else [] end)"' > ${OUTPUT}

while read line
do
    pid=$(echo ${line} | cut -d'|' -f1 | cut -d: -f2)
    req="$(echo ${line} | cut -d'|' -f2 | jq -R '.' | jq -R '.')"
    echo curl -X PUT localhost:${CARMA_PORT}/_/partner/${pid} --data "'"'{"phones":'${req}'}'"'" | grep -v "\[\]" >> ${OUTPUT2}
done < ${OUTPUT}

less ${OUTPUT2}
bash ${OUTPUT2}
rm ${OUTPUT}
rm ${OUTPUT2}
