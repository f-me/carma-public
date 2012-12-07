#!/bin/bash

declare -A fields=(
    ["car_vin"]=1
    ["car_seller"]=1
    ["car_make"]=1
    ["car_model"]=1
    ["car_plateNum"]=1
    ["car_makeYear"]=1
    ["car_color"]=1
    ["car_buyDate"]=1
    ["car_checkupDate"]=1
    ["car_dealerTO"]=1
    ["car_mileage"]=1
    ["car_checkupMileage"]=1
    ["car_transmission"]=1
    ["car_engine"]=1
    ["car_liters"]=1
    ["car_capacity"]=1
    ["car_dims"]=1
    ["car_weight"]=1
    ["car_checkPeriod"]=1
    ["car_class"]=1
    ["car_makeCode"]=1
    ["car_modelCode"]=1
    ["car_faultCode"]=1
    ["contact_name"]=1
    ["contact_phone1"]=1
    ["contact_phone2"]=1
    ["contact_phone3"]=1
    ["contact_phone4"]=1
    ["contact_email"]=1
    ["contact_contactOwner"]=1
    ["contact_ownerName"]=1
    ["contact_ownerPhone1"]=1
    ["contact_ownerPhone2"]=1
    ["contact_ownerPhone3"]=1
    ["contact_ownerPhone4"]=1
    ["contact_ownerEmail"]=1
    ["cardNumber_cardNumber"]=1
    ["cardNumber_validFrom"]=1
    ["cardNumber_validUntil"]=1
    ["cardNumber_validUntilMilage"]=1
    ["cardNumber_milageTO"]=1
    ["cardNumber_serviceInterval"]=1
    ["cardNumber_cardOwner"]=1
    ["cardNumber_manager"]=1
    ["program"]=1
)

for v in `redis-cli keys vin:*`
do
    for k in `redis-cli hkeys $v`
    do
        if ! [[ ${fields[$k]} ]]
        then
            redis-cli hdel $v $k
        fi
    done
done
