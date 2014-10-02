#!/bin/bash -e
redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'action:*')) do  redis.call('del', k) end" 0
EOF
