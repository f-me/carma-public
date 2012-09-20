#! /bin/bash

for p in `redis-cli keys partner:*` ; do
  a=`redis-cli hget $p isActive`
  d=`redis-cli hget $p isDealer`
  redis-cli hmset $p isActive ${a:-0} isDealer ${d:-0}
done
