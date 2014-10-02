#!/bin/bash -e
redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'sms:*')) do  redis.call('del', k) end" 0
EOF
