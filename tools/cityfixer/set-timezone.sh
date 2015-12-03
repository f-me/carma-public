#! /usr/bin/env bash


# Delay between consecutive requests to Google API
DELAY=0.5

GOOGLE_TZ_HTTP="https://maps.googleapis.com/maps/api/timezone/json"


Q="select
    id,
    ST_X(coords),
    ST_Y(coords)
  from \"City\"
  where coalesce(timezone, '') = ''"

psql carma -t -A -F' ' -c "$Q" | while read city; do
  ARGS=($city)
  ID=${ARGS[0]}
  LON=${ARGS[1]}
  LAT=${ARGS[2]}
  TZ=$(curl -Gs "${GOOGLE_TZ_HTTP}" \
      --data "sensor=false" \
      --data "location=${LAT},${LON}" \
      --data "timestamp=`date +'%s'`" | \
      jq -r '.timeZoneId')
  sleep $DELAY
  psql carma -t -c "update \"City\"
    set timezone='$TZ'
    where id=$ID
    returning (label, timezone)"
done
