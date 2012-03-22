#!/bin/bash
echo flushall
redis-cli flushall
echo loading data
./dist/build/hmset-poster/hmset-poster -c $1 -i caller_ownerName -i callDate -i car_plateNum -i caller_phone -i program && echo OK
