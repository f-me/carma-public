#!/bin/bash -e
redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'call:*')) do  redis.call('del', k) end" 0
EOF
