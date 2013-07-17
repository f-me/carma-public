
#!/bin/bash -e

LUA=$(cat <<EOF
for _,k in pairs(redis.call('keys', 'usermeta:*')) do
  local f = redis.call('hget', k, 'roles')
  redis.call('hset', k, 'roles', 'all,' .. f)
end
EOF
)

redis-cli eval "${LUA}" 0

SQL=$(cat <<EOF
update usermetatbl
  set roles = roles || ARRAY ['all']
  where not 'all' = any(roles)
EOF
)

$PSQL -c "${SQL}"
