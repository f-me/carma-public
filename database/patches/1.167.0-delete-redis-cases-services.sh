#!/bin/bash -e
redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'case:*')) do  redis.call('del', k) end" 0
EOF

redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'towage:*')) do  redis.call('del', k) end" 0
EOF

redis-cli << EOF
  eval "for _,k in pairs(redis.call('keys', 'tech:*')) do  redis.call('del', k) end" 0
EOF
