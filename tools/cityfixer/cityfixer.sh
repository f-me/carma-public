#! /usr/bin/env bash

# --------------------------------------
#
# Update city coordinates and timezone data in CaRMa.
#
# Usage:
#
#    CARMA_PORT=[CaRMa port number] bash cityfixer.sh
#
#    If no port specified, then default value of 8000
#    (see CARMA_DEFAULT_PORT var) is used.
#
# Output files:
#
# 1. cityfixer-****.result.txt
#
#    id|city|lon|lat|tz
#
# 2. cityfixer-****.log
#
#    Readable processing log (also shown in pager when the script
#    finishes)


### CONFIG ###
# Delay between consecutive geocoding requests

DELAY=0.5

GOOGLE_GEOCODE_HTTP="http://maps.googleapis.com/maps/api/geocode/json"
GOOGLE_TZ_HTTP="https://maps.googleapis.com/maps/api/timezone/json"

CITIES_HTTP1="http://localhost:"
CARMA_DEFAULT_PORT=8000
CITIES_HTTP2="/all/city?fields=id,city"

### END OF CONFIG ###

command-exists-p () {
    type -P $1 >/dev/null || { echo "$1: command not found" && exit 127; }
}

command-exists-p jq
command-exists-p curl
command-exists-p ${PAGER}

# Set default CaRMa port when no port is set from environment
if [ -z $CARMA_PORT ]
then
    CARMA_PORT=${CARMA_DEFAULT_PORT}
fi

CITIES_HTTP="${CITIES_HTTP1}${CARMA_PORT}${CITIES_HTTP2}"
echo "Using '${CITIES_HTTP}' as CaRMa API endpoint"

LOG=$(mktemp cityfixer-XXXXXX.log)
SOURCE_LIST=${LOG%log}source.txt
RES_LIST=${LOG%log}result.txt

log () {
    echo $1
    echo $1 >> ${LOG}
}

dest () {
    echo $1 | sed -e "s!'!!" >> ${RES_LIST}
}

curl ${CITIES_HTTP} 2>/dev/null | \
    jq -r '.[] | .id + "|" + .city' | \
    cut -d: -f2 > ${SOURCE_LIST}

echo "Writing result list to ${RES_LIST}"

while read line
do
    id=$(echo ${line} | cut -d'|' -f1)
    city=$(echo ${line} | cut -d'|' -f2)

    log "${id}: city='${city}'"
    if [ -z "${city}" ]
    then
        log "${id}: no city, skipping"
        continue
    fi

    # Geocode city to obtain coordinates
    res=$(curl -Gs "${GOOGLE_GEOCODE_HTTP}" \
        --data "address=${city}" \
        --data "sensor=false" | \
        jq -r '.results[0].geometry.location')

    if [ "${res}" = "null" ]
    then
        log "${id}: ${city}: geocoding failed"
        dest "${id}||||"
    else
        # Extract lat-lon
        lon=$(echo ${res} | jq -r '.lng')
        lat=$(echo ${res} | jq -r '.lat')

        log "${id}: target acquired at lon=${lon}, lat=${lat}"

        # Obtain timezone by coordinates
        tz=$(curl -Gs "${GOOGLE_TZ_HTTP}" \
            --data "sensor=false" \
            --data "location=${lat},${lon}" \
            --data "timestamp=`date +'%s'`" | \
            jq -r '.timeZoneId')
        
        log "${id}: timezone is ${tz}"
        dest "${id}|${city}|${lon}|${lat}|${tz}"
    fi
    log ""
    sleep ${DELAY}
done < ${SOURCE_LIST}

${PAGER} ${LOG}
