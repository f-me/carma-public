#!/bin/bash -e

SCRIPT=<<EOF
for _,k in pairs(redis.call('keys', '*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    f = redis.call('hget', k, 'files')
    if f and not string.find(f, 'attachment:') then
      redis.call('hdel', k, 'files')
    end
  end
end

for _,k in pairs(redis.call('keys', 'program:*')) do
  local r = redis.call('type', k)
  if r.ok == 'hash' then
    r = redis.call('hget', k, 'contracts')
    if r and not string.find(r, 'attachment:') then
      redis.call('hdel', k, 'contracts')
    end
    l = redis.call('hget', k, 'logo')
    if l and not string.find(l, 'attachment:') then
      redis.call('hdel', k, 'logo')
    end
  end
end
EOF

redis-cli eval "${SCRIPT}" 0
