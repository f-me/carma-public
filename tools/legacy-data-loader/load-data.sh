#!/bin/bash
if [[ $# -ne "3" ]]
then
    echo "Usage: ./load-data.sh [HMSET-POSTER-file] [number-of-threads] [LDL-directory]"
    exit 1
fi
echo flushall
redis-cli flushall
echo loading data
./dist/build/hmset-poster/hmset-poster -c $1 $2 && echo OK
echo "loading partners & VINs"
./dist/build/ldl/ldl $3 && echo OK
