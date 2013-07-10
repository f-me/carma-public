#!/bin/bash -e

export DB_NAME=${2:-carma}
export PSQL="psql $DB_NAME -v ON_ERROR_STOP=1"


function get_db_version {
  local QUERY="select A, B, C from version order by A, B, C desc limit 1;"
  local VERSION=`$PSQL -t -c "$QUERY" | tr -d ' '`

  local AB=${VERSION#*|}
  DB_VERSION_A=${VERSION%%|*}
  DB_VERSION_B=${AB%%|*}
  DB_VERSION_C=${AB#*|}

  DB_VERSION="$DB_VERSION_A.$DB_VERSION_B.$DB_VERSION_C"

  if [[ ! $DB_VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] ; then
    echo !!! Invalid DB version $DB_VERSION >&2
    exit 1
  fi

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
  createdb $DB_NAME
  for f in `find baseline -type f | sort -t'/' -k 2,2n -k 3,3n` ; do
    exec_file $f
  done
  get_db_version
}


function update_db {
  get_db_version
  for f in `find patches -type f | sort -t. -k 1,1n -k 2,2n -k 3,3n` ; do
    get_patch_version $f
    if [[ "$PATCH_VERSION" > "$DB_VERSION" ]] ; then
      exec_file $f

      local P_SQL_VER="($PATCH_VERSION_A,$PATCH_VERSION_B,$PATCH_VERSION_C)"
      $PSQL -c "insert into version (A,B,C) values $P_SQL_VER;"
    fi
  done
  get_db_version
}


function db_stat {
  cat << EOF | $PSQL
    with statio as (
      select
        (heap_blks_hit + heap_blks_read) as heap_reads,
        heap_blks_hit as heap_hits,
        relid
        from pg_statio_user_tables)
    select
      t.relname,
      pg_size_pretty(pg_relation_size(t.relid)) as table_size,
      pg_size_pretty(pg_total_relation_size(t.relid)) as total_size,
      seq_scan, idx_scan,
      heap_reads,
      case when heap_reads <> 0
        then round(heap_hits / heap_reads, 2)
        else null
      end as hit_rate
    from pg_stat_user_tables t join statio io
      on t.relid = io.relid
    order by pg_relation_size(t.relid) desc
    limit 10;
EOF

  cat << EOF | $PSQL
    with statio as (
      select
        (idx_blks_hit + idx_blks_read) as idx_reads,
        idx_blks_hit as idx_hits,
        indexrelid
        from pg_statio_user_indexes)
    select
      relname,
      indexrelname,
      pg_size_pretty(pg_relation_size(t.indexrelid)) as index_size,
      idx_scan,
      idx_tup_read,
      idx_tup_fetch,
      idx_reads,
      case when idx_reads <> 0
        then round(idx_hits / idx_reads, 2)
        else null
      end as hit_rate
    from pg_stat_user_indexes t join statio
      on t.indexrelid = statio.indexrelid
    order by pg_relation_size(t.indexrelid) desc
    limit 10;
EOF

  # Modified copypaste from http://stackoverflow.com/questions/934360
  cat << EOF | $PSQL
    select case
           when pg_buffercache.reldatabase = 0
                then '- global'
           when pg_buffercache.reldatabase <> (select pg_database.oid from pg_database where pg_database.datname = current_database())
                then '- database ' || quote_literal(pg_database.datname)
           when pg_namespace.nspname = 'pg_catalog'
                then '- system catalogues'
           when pg_class.oid is null and pg_buffercache.relfilenode > 0
                then '- unknown file ' || pg_buffercache.relfilenode
           when pg_namespace.nspname = 'pg_toast' and pg_class.relname ~ '^pg_toast_[0-9]+$'
                then (substring(pg_class.relname, 10)::oid)::regclass || ' TOAST'::text
           when pg_namespace.nspname = 'pg_toast' and pg_class.relname ~ '^pg_toast_[0-9]+_index$'
                then ((rtrim(substring(pg_class.relname, 10), '_index'))::oid)::regclass || ' TOAST index'
           else pg_class.oid::regclass::text
           end as key,
           round(avg(usagecount), 2) as usage,
           count(*) as buffers,
           sum(case when pg_buffercache.isdirty then 1 else 0 end) as dirty_buffers,
           round(count(*) / (SELECT pg_settings.setting FROM pg_settings WHERE pg_settings.name = 'shared_buffers')::numeric, 2) as hog_factor
    from pg_buffercache
         left join pg_database on pg_database.oid = pg_buffercache.reldatabase
         left join pg_class on pg_class.relfilenode = pg_buffercache.relfilenode
         left join pg_namespace on pg_namespace.oid = pg_class.relnamespace
    group by key
    order by buffers desc
    limit 10;
EOF
}


if [[ "$1" == "setup" ]] ; then
  setup_db
elif [[ "$1" == "update" ]] ; then
  update_db
elif [[ "$1" == "stat" ]] ; then
  db_stat
else
  echo "Usage: ./db.sh [setup|update|stat] db_name"
fi
