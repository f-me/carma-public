#!/bin/sh

for i in `redis-cli keys partner_service:*`; do
    p1=`redis-cli hget $i priority1`
    p2=`redis-cli hget $i priority2`
    p3=`redis-cli hget $i priority3`

    if [[ -n $p1 ]]; then
        echo $p1 | sed 's/Приоритет//' | xargs redis-cli hset $i priority1
    fi
    if [[ -n $p2 ]]; then
        echo $p2 | sed 's/Приоритет//' | xargs redis-cli hset $i priority2
    fi
    if [[ -n $p3 ]]; then
        echo $p3 | sed 's/Приоритет//' | xargs redis-cli hset $i priority3
    fi

done
