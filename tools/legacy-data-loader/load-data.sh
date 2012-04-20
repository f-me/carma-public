#!/bin/bash
if [[ $# -ne "2" ]]
then
    echo "Usage: ./load-data.sh [HMSET-POSTER-file] [LDL-directory]"
    exit 1
fi
echo flushall
redis-cli flushall
echo loading data
./dist/build/hmset-poster/hmset-poster -c $1 && echo OK
echo "loading partners & VINs"
./dist/build/ldl/ldl $2 && echo OK
