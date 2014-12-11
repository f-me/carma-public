#!/bin/bash -e

# ARC VIN automatic import script.
#
# Usage:
#
#     ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID
#
# Must be run from ~carma directory.
#
# sftp(1), unzip(1) and sshpass(1) must be available.
#
# .netrc file must contain SFTP credentials for ARC host in one line.

if [ $# -ne 2 ]
then
    echo "Usage: ./arc-vin-import.sh FTP-FILE-NAME SUBPROGRAM-ID"
    exit 1
fi

# ARC VinFormat id
FORMAT="1000"
# Contract commiter (PSA user)
COMMITTER="387"

NAME="$1"
SUBPROGRAM="$2"

# ARC host data
USER="ramc"
HOST="arcftp.arceurope.com"

# CaRMa Postgres connection info
PG="localhost,5432,carma_db_sync,pass,carma"

DIR="Production/Vehicle_info/Common"

# Absolute path to vinnie executable
VINNIE="${HOME}/carma/srv/.cabal-sandbox/bin/vinnie"

TMPDIR=$(mktemp -d /tmp/arcXXXXXX)
TMP="${TMPDIR}/${NAME}"

# Download VIN database
echo "get ${DIR}/${NAME} ${TMP}" | sshpass -p $(grep ${HOST} ~/.netrc | cut -d' ' -f6) sftp ${USER}@${HOST}

# Unpack single file from the archive
NAME="${TMPDIR}/$(unzip -Z -1 ${TMP})"
unzip ${TMP} -d ${TMPDIR}

IN=${NAME%csv}RU.csv
OUT=${IN%csv}OUT.csv

# Header row
head -n 1 ${NAME} > ${IN}
# Filter out non-RU rows
grep -E '^([^;]*;){12}RU' ${NAME} >> ${IN}

# Run vinnie
${VINNIE} -c ${PG} \
          -s ${SUBPROGRAM} \
          ${IN} ${OUT} ${COMMITTER} ${FORMAT}

# Dump output and result file name
echo ${OUT}
