#!/bin/bash -e

NUM_OF_PARALLEL_PROCESSES=$1

CMD_SET="echo ."
IX=0

while read cmd ; do
  IX=$((IX+1))
  CMD_SET="$CMD_SET & $cmd"
  if [[ "$IX" -eq "$NUM_OF_PARALLEL_PROCESSES" ]] ; then
    bash -c "$CMD_SET"
    IX=0
    CMD_SET="echo ."
  fi
done < /proc/${$}/fd/0

bash -c "$CMD_SET"
