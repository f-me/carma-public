#!/bin/bash
echo flushall
redis-cli flushall
echo loading data
./dist/build/hmset-poster/hmset-poster -c $1 -i caller:ownerName -i callDate -i car:plateNum -i caller:phone -i program && echo OK
