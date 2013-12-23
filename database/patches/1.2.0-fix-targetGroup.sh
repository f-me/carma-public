#!/bin/bash -e

$PSQL <<EOF | bash
select
  'curl -X PUT localhost:8000/_/action/'
    || id
    || ' --data ''{"targetGroup":"23"}'''
  from actiontbl where targetGroup = 'back';
EOF
