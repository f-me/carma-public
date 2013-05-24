#!/bin/bash -e

export DB_NAME=carma
export PSQL="psql $DB_NAME -v ON_ERROR_STOP=1"


function get_db_version {
  local QUERY="select A, B, C from version order by A, B, C desc limit 1;"
  local VERSION=`$PSQL -t -c "$QUERY" | tr -d ' '`

  local AB=${VERSION#*|}
  DB_VERSION_A=${VERSION%%|*}
  DB_VERSION_B=${AB%%|*}
  DB_VERSION_C=${AB#*|}

  DB_VERSION="$DB_VERSION_A.$DB_VERSION_B.$DB_VERSION_C"

  echo === Current DB version is \'$DB_VERSION\'.
}

function get_patch_version {
  local FULL_PATH=$1
  local FILE=${FULL_PATH##*/}
  PATCH_VERSION=${FILE%%-*}

  if [[ ! $PATCH_VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ; then
    echo !!! Invalid patch name $FULL_PATH >&2
    exit 1
  fi

  local AB=${PATCH_VERSION#*.}
  PATCH_VERSION_A=${PATCH_VERSION%%.*}
  PATCH_VERSION_B=${AB%%.*}
  PATCH_VERSION_C=${AB#*.}
}


function exec_file {
  local FILE=$1
  echo ... $FILE
  ext=${FILE##*.}
  if [[ "sql" == $ext ]] ; then
    $PSQL -f $FILE
  elif [[ "sh" == $ext ]] ;  then
    bash $FILE
  fi
}


function setup_db {
  for f in `find baseline -type f | sort` ; do
    exec_file $f
  done
  get_db_version
}


function update_db {
  get_db_version
  for f in `find patches -type f | sort` ; do
    get_patch_version $f
    if [[ "$PATCH_VERSION" > "$DB_VERSION" ]] ; then
      exec_file $f

      local P_SQL_VER="($PATCH_VERSION_A,$PATCH_VERSION_B,$PATCH_VERSION_C)"
      $PSQL -c "insert into version (A,B,C) values $P_SQL_VER;"
    fi
  done
  get_db_version
}


if [[ "$1" == "setup" ]] ; then
  setup_db
elif [[ "$1" == "update" ]] ; then
  update_db
else
  echo "Usage: ./db.sh [setup|update]"
fi
