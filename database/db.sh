#!/bin/bash -e

export DB_NAME=${2:-carma}
export PSQL="psql $DB_NAME -v ON_ERROR_STOP=1"


function get_db_version {
  local QUERY="select A, B, C from version order by A desc, B desc, C desc limit 1;"
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
  if [[ $2 != "--dev" ]]; then
      # Checkout database version specified by #CHECKOUT=<commit>/<file>
      COMMIT=$(grep '#CHECKOUT=' ${FILE} | sed -e 's/#CHECKOUT=\([[:alnum:]]\+\)\/.\+$/\1/')
      OLDNAME=$(grep '#CHECKOUT=' ${FILE} | sed -e 's/#CHECKOUT=.\+\/\(.\+\)$/\1/')
      # Otherwise checkout last version the file was changed in
      if [[ -z $COMMIT ]]; then
          COMMIT=$(git log -n 1 --no-merges --pretty=format:%h -- ${FILE})
      else
          # Use file name from #CHECKOUT line
          if [[ -n $OLDNAME ]]; then
              FILE="patches/${OLDNAME}"
          fi
      fi
      TMP=$(mktemp -d)
      cd ..
      git --work-tree="${TMP}" checkout ${COMMIT} -- database/
      cd "${TMP}/database"
      echo ... using snapshot from ${COMMIT}
  fi
  if [[ "sql" == $ext ]] ; then
    $PSQL -f $FILE
  elif [[ "sh" == $ext ]] ;  then
    bash -e $FILE
  fi
  if [[ $2 != "--dev" ]]; then
     cd - >/dev/null
     cd database
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
  local A=$DB_VERSION_A
  local B=$DB_VERSION_B
  local C=$DB_VERSION_C
  local DUPE=0

  for d in `find patches -type f | sort -V | cut -d'-' -f1 | uniq -d` ; do
    echo \*\*\* Multiple patches use version $(echo ${d} | cut -d'/' -f2)
    DUPE=1
  done

  if [[ ${DUPE} == 1 ]] ; then
    if [[ $1 != "--dev" ]]; then
      echo !!! Resolve duplicates and try again
      exit 1
    fi
  fi

  if [[ $2 != "--dev" ]]; then
    ORIGROOT=${PWD}
    TMPROOT=$(mktemp -d)
    git clone --single-branch .. ${TMPROOT}
    cd ${TMPROOT}/database
  fi

  for f in `find patches -type f | sort -V` ; do
    get_patch_version $f
    local X=$PATCH_VERSION_A
    local Y=$PATCH_VERSION_B
    local Z=$PATCH_VERSION_C

    local count=$(${PSQL} -t -c "select count(1) from version where (A,B,C)=($X,$Y,$Z);" | tr -d ' ' | head -n 1)
    if [[ $count == "0" ]] ; then
      if [[ ($X -lt $A) ||
            (($X -eq $A) && ($Y -lt $B)) ||
            (($X -eq $A) && ($Y -eq $B) && ($Z -lt $C)) ]] ; then
        if [[ $1 != "--dev" ]]; then
          echo \*\*\* Patch $(echo ${f} | cut -d'/' -f2) was left behind
          echo \*\*\* Apply it?
          select yn in "Yes" "No"; do
            case $yn in
              Yes ) break;;
              No ) continue 2;;
            esac
          done
        else
          continue 2
        fi
      fi
      exec_file $f $1

      $PSQL -c "insert into version (A,B,C) values ($X,$Y,$Z);"
    fi
  done
  if [[ $2 != "--dev" ]]; then
      cd ${ORIGROOT}
      rm -rf ${TMPROOT}
  fi
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
elif [[ "$1" == "update-devel" ]] ; then
  update_db --dev
elif [[ "$1" == "stat" ]] ; then
  db_stat
else
  echo "Usage: ./db.sh [setup|update-devel|update|stat] db_name"
fi
