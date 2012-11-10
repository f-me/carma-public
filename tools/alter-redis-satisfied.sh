#!/bin/sh

list="tech towage rent hotel taxi sober transportation deliverCar deliverParts ken tech1 information tickets unfuel continue bank deliverClient averageCommissioner insurance";

for i in $list
do
    for j in `redis-cli keys $i:*`
    do
        var=`redis-cli hget $j clientSatisfied`
        if [ "$var" = "1" ]
        then
            redis-cli hmset $j clientSatisfied satis
        elif [ "$var" = "0" ]
        then
            redis-cli hmset $j clientSatisfied notSatis
        fi
    done
done
