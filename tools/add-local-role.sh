#!/bin/bash

# Add role "local" to all users.
#
# Usage: ./add-local-role.sh users.json

NEWFILE=$(mktemp users-XXXXXX.json)

cat $1 | sed -e 's/roles":\[/roles":\["local",/g' > ${NEWFILE}

echo "Please find your new users.json file at ${NEWFILE}"
