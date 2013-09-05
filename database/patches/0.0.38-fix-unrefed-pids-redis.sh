#!/bin/bash -e

read -r -d '' SCRIPT <<EOF
for _,k in pairs(redis.call('keys', 'towage:*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    r = redis.call('hget', k, 'towDealer_partnerId')
    if r and not string.find(r, ':') and not (r == '') then
      redis.call('hset', k, 'towDealer_partnerId', 'partner:' .. r)
    end
  end
end

for _,k in pairs(redis.call('keys', 'towage:*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    r = redis.call('hget', k, 'contractor_partnerId')
    if r and not string.find(r, ':') and not (r == '') then
      redis.call('hset', k, 'contractor_partnerId', 'partner:' .. r)
    end
  end
end

for _,k in pairs(redis.call('keys', 'tech:*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    r = redis.call('hget', k, 'contractor_partnerId')
    if r and not string.find(r, ':') and not (r == '') then
      redis.call('hset', k, 'contractor_partnerId', 'partner:' .. r)
    end
  end
end
EOF

redis-cli eval "${SCRIPT}" 0
