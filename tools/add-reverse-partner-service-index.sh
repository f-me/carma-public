#!/bin/sh

for partner in `redis-cli keys "partner:*"`; do
    for partner_service in `redis-cli hget ${partner} services | sed 's/,/ /g'`; do
        redis-cli hset ${partner_service} parentId ${partner}
    done
done

for partner_service in  `redis-cli keys "partner_service:*"`; do
    serviceName=`redis-cli hget ${partner_service} serviceName`
    cash=`echo ${partner_service} | sed 's/partner_service://'`
    redis-cli sadd "partner_service:serviceName:${serviceName}" ${cash}
done
