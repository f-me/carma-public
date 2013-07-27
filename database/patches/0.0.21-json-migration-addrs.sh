#!/bin/bash -e

export CARMA_PORT=${CARMA_PORT:=8000}

OUTPUT=$(mktemp /tmp/addrsXXXXXX-tmp)
OUTPUT2=$(mktemp /tmp/addrsXXXXXX-out)

curl -s "http://localhost:${CARMA_PORT}/all/partner" | \
    jq -c -r '.[] | .id + "|\(if .addrDeJure and ((.addrDeJure) == "" | not) then [{key:("jure"), value:.addrDeJure}] else [] end + if .addrDeFacto and ((.addrDeFacto) == "" | not) then [{key:("fact"), value:.addrDeFacto}] else [] end + if .salesAddress and ((.salesAddress) == "" | not) then [{key:("sales"), value:.salesAddress}] else [] end + if .serviceAddress and ((.serviceAddress) == "" | not) then [{key:("serv"), value:.serviceAddress}] else [] end)"' > ${OUTPUT}

while read line
do
    pid=$(echo ${line} | cut -d'|' -f1 | cut -d: -f2)
    req="$(echo ${line} | cut -d'|' -f2 | jq -R '.')"
    echo curl -X PUT localhost:${CARMA_PORT}/_/partner/${pid} --data "'"'{"addrs":'${req}'}'"'" | grep -v "\[\]" >> ${OUTPUT2}
done < ${OUTPUT}

less ${OUTPUT2}
bash ${OUTPUT2}
rm ${OUTPUT}
rm ${OUTPUT2}
