#!/bin/bash

# Transfer users.json file to Postgres
#
# Usage:
# 1) $ ./users2postgres.sh path/to/users.json
# 
# 2) When the script finishes, run .http.sh and .sql files:
#   2.1) bash users-xxxx.http.sh
#   2.2) sudo -u postgres psql -d carma -f users-xxx.sql

CARMA_PORT=8000

users=$1

outfile=$(mktemp users-XXXXXX)

rm ${outfile}
http_commands="${outfile}.http.sh"
sql_commands="${outfile}.sql"

# Create users & metas
cat ${users} | jq '.uidCache | .[][1] | {login: .login, password: (null | tostring), realName: .meta.realName, roles: (.roles | map (.+",") | add )} | tostring' | sed -e 's/,\\"/\\"/' | sed -e "s/\(.*\)/curl -X POST localhost:${CARMA_PORT}\/_\/usermeta\/ --data \1/" > ${http_commands}
# Update passwords directly in Postgres
cat ${users} | jq -r ".uidCache | .[][1] | \"UPDATE snap_auth_user SET password = '\" + .pw + \"' WHERE login = '\" + .login + \"';\"" > ${sql_commands}

echo "Run ${http_commands}, then execute ${sql_commands}"
