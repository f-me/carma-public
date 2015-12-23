#!/usr/bin/env bash

set -e

OVERPASS="http://overpass-api.de/api/interpreter"
TMP='overpass-data'
rm -rf $TMP
mkdir $TMP

for LAT in {55,75} ; do
  for LON_ in {0..16} ; do
    LON=$(($LON_ * 10))
    RECT="($(($LAT-20)),$(($LON-10)),$LAT,$LON)"
    QUERY="[out:json]; node ['place'='city'] $RECT; out body;"
    FILE="$TMP/$LON,$LAT.json"
    curl -G -s $OVERPASS --data-urlencode "data=$QUERY" > $FILE
    echo $FILE >&2
    jq -c '.elements[]
      | { lat, lon
        , label: .tags."name:ru"
        , value: (.tags."name:en" // .tags.name)
        , pop: .tags.population
        , country: (.tags."addr:country" // .tags."is_in:country" // .tags.is_in)
        }
      | select(.label and .value and .country)' \
      $FILE
  done
done
