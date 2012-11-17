#!/bin/bash

services="tech towage rent hotel taxi sober transportation deliverCar deliverParts ken tech1 information bank tickets continue bank deliverClient averageCommissioner insurance";
fields="carProvidedFor orderNumber payment_partnerCost suburbanMilage times_factServiceEnd times_factServiceStart times_repairEndDate";

echo "actions..."
for i in `redis-cli keys action:*`
do
    if [[ $i =~ ^[a-z]+:[0-9]+$ ]]
    then
        var=`redis-cli hget $i duetime`
        if ! [[ $var =~ ^[0-9]*$ ]]
        then
            #echo "$i duetime = $var and is NOT ok"
            redis-cli hmset $i duetime ""
        fi
    fi
done

echo "cases..."
for i in `redis-cli keys case:*`
do
    if [[ $i =~ ^[a-z]+:[0-9]+$ ]]
    then
        var=`redis-cli hget $i car_buyDate`
        if ! [[ $var =~ ^[0-9]*$ ]]
        then
            # echo "$i car_buyDate = $var and is NOT ok"
            redis-cli hmset $i car_buyDate ""
        fi
    fi
done

for i in $services
do
    echo "service $i..."
    for j in `redis-cli keys $i:*`
    do
        if [[ $j =~ ^[a-z]+:[0-9]+$ ]]
        then
            for k in $fields
            do
                var=`redis-cli hget $j $k`
                if ! [[ $var =~ ^[0-9]*$ ]]
                then
                    # echo "$j $k = $var and is NOT ok"
                    redis-cli hmset $j $k ""
                fi
            done
        fi
    done
done
