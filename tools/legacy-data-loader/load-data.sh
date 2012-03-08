#!/bin/bash
echo flushall
redis-cli flushall
echo loading data
./dist/build/hmset-poster/hmset-poster -c $1 -i ownerName -i callDate -i plateNum -i phone -i program && echo OK
