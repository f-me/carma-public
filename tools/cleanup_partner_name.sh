#!/bin/sh

for i in `redis-cli keys partner:*`; do
    echo $i `redis-cli hget $i name`;
done | grep --color=never $'\r' |
     tr -d $'\r'                |
     awk  '{ system("redis-cli hset " $1 " name " "\"" $2 "\"") }'
