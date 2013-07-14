#!/bin/bash -e

SCRIPT=$(cat <<EOF
for _,k in pairs(redis.call('keys', '*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    local x = redis.call('hget', k, 'files')
    if x and not string.find(x, 'attachment:') then
      redis.call('hdel', k, 'files')
    end
    x = redis.call('hget', k, 'contracts')
    if x and not string.find(x, 'attachment:') then
      redis.call('hdel', k, 'contracts')
    end
    x = redis.call('hget', k, 'logo')
    if x and not string.find(x, 'attachment:') then
      redis.call('hdel', k, 'logo')
    end
  end
end
EOF
)

redis-cli eval "${SCRIPT}" 0
