#!/bin/bash

declare -A renames=(
    ["vin"]="car_vin"
    ["make"]="car_make"
    ["model"]="car_model"
    ["plateNum"]="car_plateNum"
    ["makeYear"]="car_makeYear"
    ["color"]="car_color"
    ["buyDate"]="car_buyDate"
    ["checkupDate"]="car_checkupDate"
    ["cardNumber"]="cardNumber_cardNumber"
    ["validFrom"]="cardNumber_validFrom"
    ["validUntil"]="cardNumber_validUntil"
    ["validUntilMilage"]="cardNumber_validUntilMilage"
    ["milageTO"]="cardNumber_milageTO"
    ["serviceInterval"]="cardNumber_serviceInterval"
    ["cardOwner"]="cardNumber_cardOwner"
    ["manager"]="cardNumber_manager"
    ["owner_name"]="contact_ownerName"
    ["owner_phone1"]="contact_ownerPhone1"
    ["owner_email"]="contact_ownerEmail"
)

i=0
ks=`redis-cli keys vin:*`
cnt=`echo "$ks" | wc -l`

for v in $ks
do
    echo "$i/$cnt"
    i=$((i+1))
    for k in `redis-cli hkeys $v`
    do
        renameto=${renames[$k]}
        if [[ $renameto ]]
        then
            val=`redis-cli hget $v $k`
            redis-cli hset $v $renameto "$val"
            redis-cli hdel $v $k
        fi
    done
done
