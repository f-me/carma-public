#! /usr/bin/env bash

# --------------------------------------
# 
# Import partner data from CaRMa and attempt to geocode. 
#
# Usage:
#
#    bash geofix.sh [CaRMa port number]
#
#    If no port specified, then default value of 8000 is used.
#
# Output files:
#
# 1. geofix-****.result.txt
#    id|name|city|address|lon|lat 
#
# 2. geofix-****.log
#
#    Readable processing log
#
# 3. geofix-****.source.txt
#    id|name|city|address
#
#    Intermediate list of partners as stored in Redis
#
# Certified for GNU Enterprise Bash 2012
#
# --------------------------------------

# Delay between consecutive geocoding requests

DELAY=0.1

YANDEX_HTTP="http://geocode-maps.yandex.ru/1.x/"

PARTNERS_HTTP1="http://localhost:"
PARTNERS_PORT="8000"
PARTNERS_HTTP2="/all/partner?fields=id,name,city,addrDeFacto"

POSTAL_DB="./RU.txt"

command-exists-p () {
    type -P $1 >/dev/null || { echo "$1: command not found" && exit 127; }
}


file-exists-p () {
    if [ ! -f "$1" ] 
    then
        echo "$1: file not found" && exit 1
    fi
}

lookup-code () {
    grep $1 ${POSTAL_DB} | cut -f3 | sed -e "s/[[:digit:]]*//g"
}

command-exists-p jq
command-exists-p curl
file-exists-p ${POSTAL_DB}


# Override default CaRMa port when given a command line argument
if [ $# -gt 0 ]
then
    PARTNERS_PORT="$1"
fi

PARTNERS_HTTP="${PARTNERS_HTTP1}${PARTNERS_PORT}${PARTNERS_HTTP2}"
echo "Using '${PARTNERS_HTTP}' as CaRMa API endpoint"


LOG=$(mktemp geofix-XXXXXX.log)
SOURCE_LIST=${LOG%log}source.txt
RES_LIST=${LOG%log}result.txt
echo "Writing verbose import log at ${LOG}"

log () {
    echo $1 >> ${LOG}
}

dest () {
    echo $1 | sed -e "s!'!!" >> ${RES_LIST}
}

echo "Writing source partner list to ${SOURCE_LIST}"

# Fetch list of `id|name|city|address` entries
#
# TODO Ignore partners with non-null coords.
curl ${PARTNERS_HTTP} 2>/dev/null | \
    jq -r '.[] | .id + "|" + .name + "|" + .city + "|" + .addrDeFacto' | \
    cut -d: -f2 > ${SOURCE_LIST}

echo "Writing result list to ${RES_LIST}"

while read line
do
    log ""
    id=$(echo ${line} | cut -d'|' -f1)
    name=$(echo ${line} | cut -d'|' -f2)
    city=$(echo ${line} | cut -d'|' -f3)
    addr=$(echo ${line} | cut -d'|' -f4)

    log "${id}: name='${name}'"
    log "${id}: city='${city}', address='${addr}'"

    new_name=$(echo ${name} | sed -e 's/[[:space:]]*$//')
    log "${id}: new name='${new_name}'"

    # We expect that city and address are both specified. If not, we
    # attempt to derive city from postal code specified in the
    # address.
    if [ -z "${city}" ]
    then
        log "${id}: no city specified"
        if [ "${addr}" ]
        then
            # No city, but there's address
            log "${id}: trying to extract postal code from ${addr}"
            postal=$(echo ${addr} | grep '[[:digit:]]\{6\}' -o)
            if [ "${postal}" ]
            then
                log "${id}: found postal code ${postal}"
                city=`lookup-code ${postal}`
                if [ "${city}" ]
                then
                    log "${id}: city found by postal code: ${city}"
                else
                    log "${id}: no city found"
                fi
            else
                log "${id}: no postal code found"
            fi
        else
            # No city, no address
            log "${id}: no address either, skipping"
            dest "${id}|||||"
            continue
        fi
    fi

    # Include city in address, avoiding dupes
    if [ "${city}" ]
    then
        addr=$(echo ${addr} | sed -e "s!${city}!!" | sed -e "s!г\.!!")
        addr="${city} ${addr}"
    fi

    # Finally, attempt to geocode
    log "${id}: attempting to geocode: ${addr}"
    res=$(curl "${YANDEX_HTTP}" \
        --data "format=json" --data-urlencode "geocode=${addr}" \
        2>/dev/null | \
        jq -r '.response.GeoObjectCollection.featureMember[0].GeoObject')
    if [ "${res}" = "null" ]
    then
        log "${id}: geocoding failed"
        dest "${id}|||||"
    else
        # Extract lat-lon
        coords=$(echo ${res} | jq -r '.Point.pos')
        lon=$(echo ${coords} | cut -d' ' -f1)
        lat=$(echo ${coords} | cut -d' ' -f2)

        log "${id}: target acquired at lon=${lon}, lat=${lat}"

        # Reverse geocode city
        new_city=$(echo ${res} | \
            jq -r '.metaDataProperty.GeocoderMetaData.AddressDetails.Country | .Locality.LocalityName, .AdministrativeArea.Locality.LocalityName' | \
            grep -v 'null')

        # Use old city if no new city found (this happens)
        if [ -z "${new_city}" ]
        then
            new_city=${city}
        fi

        # Reverse geocode address
        new_addr=$(echo ${res} | \
            jq -r '.name')

        # When searching only by city, address is equal to city, so drop it
        if [ "${new_addr}" = "${new_city}" ]
        then
            new_addr=""
        fi

        log "${id}: new city='${new_city}', new address='${new_addr}'"
        # Show changes in result
        dest "${id}|${new_name}|${new_city}|${new_addr}|${lon}|${lat}"
    fi
    sleep ${DELAY}
done < ${SOURCE_LIST}
