#!/bin/bash

function rename {
  id=$1
  prefix=$2
  partner=`redis-cli hget $id ${prefix}Contractor_partner`
  address=`redis-cli hget $id ${prefix}Contractor_address`
  redis-cli \
      hmset $id \
      contractor_partner "$partner" \
      contractor_address "$address"
}

services=(hotel rent sober taxi tech towage)
for model_name in ${services[@]} ; do
  prefix=${model_name/towage/tow}
  for s in `redis-cli keys ${model_name}:*` ; do
    rename $s $prefix
    if [[ $? != 0 ]] ; then
      # redis can't handle lots of connections
      while [[ 10000 < `netstat | grep "6379\sTIME_WAIT" | wc -l` ]] ; do
        sleep 10
      done
      echo retry > /dev/stderr 
      rename $s $prefix
    fi
  done
done
