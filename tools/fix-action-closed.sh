#!/bin/sh

# turn all action closed fields from true/false -> 1/0

for i in `redis-cli keys "action:*"`; do
    redis-cli hget $i closed | \
    awk '/true/ {print 1}; /false/ {print 0};' | \
        xargs -r redis-cli hset $i closed
done