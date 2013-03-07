#!/bin/bash

# Convert redis-stored vins for a program to instances of `contract`
# model.
# 
# Usage: ./convert-vins.sh > out-vins && bash out-vins

PORT=8085
PROGRAM="ruslan"

vinlist=$(mktemp --tmpdir vinsXXXXXX.txt)
redis-cli keys "vin:*" | cut -d: -f2 > ${vinlist}
while read vin
do
    resp=$(curl -s http://localhost:${PORT}/_/vin/${vin}/)
    body=$(echo "${resp}" | grep ${PROGRAM})
    if [ -n "${body}" ]
    then
        jq_body=$(echo "${body}" | jq '{carVin: .car_vin, carSeller: .car_seller, carMake: .car_make, carModel: .car_model, carMakeYear: .car_makeYear, carColor: .car_color, carBuyDate: .car_buyDate, carTransmission: .car_transmission, cardNumber: .cardNumber_cardNumber, contractValidFromDate: .cardNumber_validFrom, contractValidUntilDate: .cardNumber_validUntil, contractValidUntilMilage: .cardNumber_validUntilMilage, milageTO: .cardNumber_milageTO, cardOwner: .contact_ownerName, manager: .cardNumber_manager, program: .program}')
        echo "curl -X POST http://localhost:${PORT}/_/contract/ --data '${jq_body}'"
    fi
done < ${vinlist}
